open O

let parse_string source =
  let lir =
    Parse.parse source |> Or_error.ok_exn |> Elaborate.elaborate |> Or_error.ok_exn
  in
  Type_check.run lir
  |> Result.map_error ~f:(fun e -> e |> Type_check.sexp_of_error |> Error.create_s)
  |> Or_error.ok_exn;
  lir
;;

let compile_lir lir =
  lir |> Lower.run |> Lir_x86.lower |> X86.Reg_alloc.run |> X86.Print.run
;;

let compile_string source =
  let lir = parse_string source in
  let asm = compile_lir lir in
  asm
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
