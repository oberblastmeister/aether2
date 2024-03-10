open Core
open Eio.Std
open Aether2.O
module Process = Eio.Process

let main ~cwd ~process ~stdout =
  Eio.Process.run ~cwd:Eio.Path.(cwd / "filetests/runtime") process [ "zig"; "build" ];
  let asm =
    Aether2.Lir.Driver.compile_string
      {|
    (define (entry) u64
      (label (start)
        (set one (const u64 1))
        (set two (const u64 2))
        (set x (call u64 (assert_u64 one two)))
        (ret)
      )
    )
    |}
  in
  let name, file = Filename_unix.open_temp_file "lir" ".s" in
  Out_channel.output_string file asm;
  Out_channel.close file;
  Eio.Flow.copy_string (asm ^ "\n") stdout;
  let path =
    Eio.Path.(cwd / "filetests/runtime/zig-out/lib/libruntime.a" |> native_exn)
  in
  Process.run process [ "zig"; "cc"; "-masm=intel"; name; path; "-o"; "lir_test" ];
  Switch.run (fun sw ->
    let proc = Process.spawn ~sw process [ "./lir_test" ] in
    let status = Process.await proc in
    (match status with
     | `Exited 0 -> ()
     | _ -> failwith "running failed");
    ())
;;

(* build the zig runtime *)
(* run each of the tests while linkinzig g it to the zig runtime *)
let () =
  Eio_main.run
  @@ fun env ->
  main
    ~cwd:(Eio.Stdenv.cwd env)
    ~stdout:(Eio.Stdenv.stdout env)
    ~process:(Eio.Stdenv.process_mgr env)
;;
