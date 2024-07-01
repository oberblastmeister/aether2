open O

type emit =
  | Lir
  | Tir
  | X86
  | Asm
[@@deriving sexp_of]

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

let compile_lir ?(emit = Asm) lir =
  let open Or_error.Let_syntax in
  match emit with
  | Asm ->
    let x86 = lir |> Split_critical.split |> Lower.run |> Lir_x86.lower in
    let%bind () = X86.Check_ssa.check x86 in
    let x86 = X86.Driver.compile_program x86 |> X86.Print.run in
    Ok x86
  | X86 ->
    let x86 = lir |> Split_critical.split |> Lower.run |> Lir_x86.lower in
    (* let%bind () = X86.Check_ssa.check x86 in *)
    Ok (Sexp.to_string_hum @@ X86.Ast.Program.sexp_of_t X86.Ast.VReg.sexp_of_t x86)
  | Lir -> Ok (Pretty.pretty lir)
  | Tir -> Ok (Lower.run lir |> Lower.Tir.pretty)
;;

let compile_string ?emit source =
  let open Result.Let_syntax in
  let%bind lir = parse_string_ssa source in
  let%bind asm = compile_lir ?emit lir in
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
