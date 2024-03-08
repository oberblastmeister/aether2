open O

let compile_string source =
  let lir =
    Parse.parse source
    |> Or_error.ok_exn
    |> Elaborate.elaborate
    |> Or_error.ok_exn
    |> Ssa.convert_ssa
    |> Lower.run
    |> Lir_x86.lower
    |> X86.Reg_alloc.run
  in
  todo ()
;;
