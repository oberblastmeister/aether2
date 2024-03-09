open O

let compile_string ~link source =
  let lir =
    Parse.parse source
    |> Or_error.ok_exn
    |> Elaborate.elaborate
    |> Or_error.ok_exn
    |> Ssa.convert_ssa
  in
  let asm = lir |> Lower.run |> Lir_x86.lower |> X86.Reg_alloc.run |> X86.Print.run in
  let name, out_channel = Filename_unix.open_temp_file "lir" "asm" in
  let _ = Io.Process.run_stdout "zig" [ "cc"; name ] in
  todo [%here]
;;

let%expect_test _ =
  let code = Sys_unix.command "echo first second third" in
  print_s [%message (code : int)];
  ();
  [%expect {|
    first second third
    (code 0) |}]
;;

let%expect_test _ =
  let output = Io.Process.run_stdout "echo" [ "first"; "second"; "third" ] in
  print_s [%message (output : string)];
  ();
  [%expect {|
    (output "first second third\n") |}]
;;
