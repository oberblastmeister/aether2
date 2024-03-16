open! Core
open Eio.Std
open Aether2.O
module Process = Eio.Process

let compile_runtime ~cwd ~process =
  Eio.Process.run ~cwd:Eio.Path.(cwd / "filetests/runtime") process [ "zig"; "build" ]
;;

let result_of_status = function
  | `Exited 0 -> Ok ()
  | `Exited n -> Error (`Exited n)
  | `Signaled n -> Error (`Signaled n)
;;

let test_header = {|
(extern (assert_eq_u64 u64 u64) void)
|}

let compile_single ~cwd ~process ~stdout ~debug path =
  let@ () = Aether2.Logger.with_log debug in
  let@ sw f = Switch.run f in
  let contents = Eio.Path.load Eio.Path.(cwd / path) in
  let asm =
    Aether2.Lir.Driver.compile_string (test_header ^ contents) |> Or_error.ok_exn
  in
  let name, file = Filename_unix.open_temp_file "lir" ".s" in
  Out_channel.output_string file asm;
  Out_channel.close file;
  Eio.Flow.copy_string (asm ^ "\n") stdout;
  let runtime_path = Eio.Path.(cwd / "filetests/runtime/zig-out/lib/libruntime.a") in
  (let stdout_buf = Buffer.create 10 in
   let stderr_buf = Buffer.create 10 in
   let proc =
     Process.spawn
       ~sw
       ~stdout:(Eio.Flow.buffer_sink stdout_buf)
       ~stderr:(Eio.Flow.buffer_sink stderr_buf)
       process
       [ "zig"; "cc"; name; Eio.Path.native_exn runtime_path ]
   in
   let status = Process.await proc |> result_of_status in
   let stderr_contents = Buffer.contents stderr_buf in
   let stdout_contents = Buffer.contents stdout_buf in
   (match status with
    | Ok () -> ()
    | Error _ ->
      Eio.Flow.copy_string ("failed to compile file " ^ path ^ "\n") stdout;
      Eio.Flow.copy_string "stderr:\n" stdout;
      Eio.Flow.copy_string stderr_contents stdout;
      ());
   ());
  let proc = Process.spawn ~sw process [ "./a.out" ] in
  let status = Process.await proc in
  (match status with
   | `Exited 0 -> ()
   | `Exited n -> print_s [%message "abnormal exit status" (n : int)]
   | `Signaled n -> print_s [%message "process was killed by a signal" (n : int)]);
  ()
;;

let run ~env (args : Args.t) =
  let cwd = Eio.Stdenv.cwd env in
  let process = Eio.Stdenv.process_mgr env in
  let stdout = Eio.Stdenv.stdout env in
  match args with
  | Compile { common = { debug }; files; _ } ->
    compile_runtime ~cwd ~process;
    List.iter files ~f:(fun file ->
      compile_single ~cwd ~stdout ~process ~debug file;
      ());
    ()
  | Test _ -> todo [%here]
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
