open! Core
open Eio.Std
open Aether2.O
module Process = Eio.Process

let ( / ) = Eio.Path.( / )

let compile_runtime ~cwd ~process =
  Eio.Process.run ~cwd:Eio.Path.(cwd / "filetests/runtime") process [ "zig"; "build" ]
;;

let result_of_status = function
  | `Exited 0 -> Ok ()
  | `Exited n -> Error (`Exited n)
  | `Signaled n -> Error (`Signaled n)
;;

let test_header =
  {|
(define (assert_eq_u64 [x : i64] [y : i64]) : void)

(define (assert_eq_i64 [x : i64] [y : i64]) : void)

(define (assert_eq_u1 [x : i1] [y : i1]) : void)

(define (print_u64 [x : i64]) : void)

(define (print_i64 [x : i64]) : void)
|}
;;

let run_file ~process path =
  let@ sw f = Switch.run f in
  let proc = Process.spawn ~sw process [ Eio.Path.native_exn path ] in
  let status = Process.await proc in
  (match status with
   | `Exited 0 -> ()
   | `Exited n -> raise_s [%message "abnormal exit status" (n : int)]
   | `Signaled n -> raise_s [%message "process was killed by a signal" (n : int)]);
  ()
;;

let compile_single ~cwd ~process ~stdout ?(debug = false) ~lir_path ~asm_path ~out_path ()
  =
  let@ () = if debug then Aether2.Logger.with_log debug else fun f -> f () in
  let@ sw f = Switch.run f in
  let contents = Eio.Path.load lir_path in
  let asm =
    Aether2.Lir.Driver.compile_string (test_header ^ contents) |> Or_error.ok_exn
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) asm_path asm;
  let runtime_path = Eio.Path.(cwd / "filetests/runtime/zig-out/lib/libruntime.a") in
  let stdout_buf = Buffer.create 10 in
  let stderr_buf = Buffer.create 10 in
  let proc =
    Process.spawn
      ~sw
      ~stdout:(Eio.Flow.buffer_sink stdout_buf)
      ~stderr:(Eio.Flow.buffer_sink stderr_buf)
      process
      [ "zig"
      ; "cc"
      ; "-fcolor-diagnostics"
      ; Eio.Path.native_exn asm_path
      ; Eio.Path.native_exn runtime_path
      ; "-o"
      ; Eio.Path.native_exn out_path
      ]
  in
  let status = Process.await proc |> result_of_status in
  let stderr_contents = Buffer.contents stderr_buf in
  let _stdout_contents = Buffer.contents stdout_buf in
  (match status with
   | Ok () -> ()
   | Error _ ->
     Eio.Flow.copy_string stderr_contents stdout;
     raise_s [%message "failed to compile file" (Eio.Path.native_exn lir_path : string)]);
  ()
;;

let run_tests ~cwd ~process ~stdout =
  let dir_path = "filetests/tests" in
  let paths =
    Eio.Path.read_dir Eio.Path.(cwd / dir_path)
    |> List.filter ~f:(String.is_suffix ~suffix:".lir")
    |> List.map ~f:(fun path ->
      ( String.chop_suffix_exn path ~suffix:".lir"
      , Eio.Path.native_exn @@ (cwd / dir_path / path) ))
  in
  let tests =
    List.map paths ~f:(fun (name, path) ->
      ( name
      , [ Alcotest.test_case "run" `Quick (fun () ->
            let asm_path = cwd / "filetests/out" / (name ^ ".s") in
            let out_path = cwd / "filetests/out" / (name ^ ".exe") in
            compile_single
              ~cwd
              ~process
              ~stdout
              ~lir_path:(cwd / path)
              ~asm_path
              ~out_path
              ();
            run_file ~process out_path;
            ())
        ] ))
  in
  Alcotest.run "Aether2" tests;
  ()
;;

let run ~env (args : Args.t) =
  let cwd = Eio.Stdenv.cwd env in
  let process = Eio.Stdenv.process_mgr env in
  let stdout = Eio.Stdenv.stdout env in
  match args with
  | Compile { common = { debug }; files; emit; _ } ->
    compile_runtime ~cwd ~process;
    List.iter files ~f:(fun file ->
      if not @@ String.is_suffix ~suffix:".lir" file
      then raise_s [%message "can only compile .lir files" (file : string)];
      let name = Filename.basename file |> Filename.chop_extension in
      (match emit with
       | None ->
         let dir_path = cwd / "lir-out" in
         if not @@ Eio.Path.is_directory dir_path
         then (
           Eio.Path.mkdir ~perm:0o700 dir_path;
           ());
         compile_single
           ~cwd
           ~stdout
           ~process
           ~debug
           ~lir_path:(cwd / file)
           ~asm_path:(dir_path / (name ^ ".s"))
           ~out_path:(dir_path / (name ^ ".exe"))
           ()
       | Some emit ->
         let contents = Eio.Path.load Eio.Path.(cwd / file) in
         let asm =
           Aether2.Lir.Driver.compile_string ~emit (test_header ^ contents)
           |> Or_error.ok_exn
         in
         Eio.Flow.copy_string (asm ^ "\n") stdout;
         ());
      ());
    ()
  | Test _ ->
    compile_runtime ~cwd ~process;
    run_tests ~cwd ~process ~stdout;
    ()
;;

let main ~env =
  match Args.main () with
  | Ok (`Ok args) -> run ~env args
  | Ok _ -> ()
  | Error _ -> exit 1
;;

(* build the zig runtime *)
(* run each of the tests while linkinzig g it to the zig runtime *)
let () =
  let@ env = Eio_main.run in
  main ~env
;;
