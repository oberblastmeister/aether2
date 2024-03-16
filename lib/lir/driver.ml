open O

let parse_string source =
  let open Result.Let_syntax in
  let%bind lir = Parse.parse source in
  let%bind lir = Elaborate.elaborate lir in
  let%bind () = Type_check.run lir in
  Ok lir
;;

let convert_ssa lir =
  let open Or_error.Let_syntax in
  let lir = Ssa.convert lir in
  [%log.global.debug (Pretty.pretty lir : string)];
  let%bind () = Check_ssa.check lir in
  Ok lir
;;

let parse_string_ssa source =
  let open Or_error.Let_syntax in
  let%bind lir = parse_string source in
  let%bind lir = convert_ssa lir in
  Ok lir
;;

let compile_lir lir =
  let open Or_error.Let_syntax in
  let x86 = lir |> Lower.run |> Lir_x86.lower in
  let%bind () = X86.Check_ssa.check x86 in
  let x86 = X86.Reg_alloc.run x86 |> X86.Print.run in
  Ok x86
;;

let compile_string source =
  let open Result.Let_syntax in
  let%bind lir = parse_string_ssa source in
  let%bind asm = compile_lir lir in
  Ok asm
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
