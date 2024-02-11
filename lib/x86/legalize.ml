open O
open Types

let legalize_instr instr ~force_same ~force_register =
  let open Types_basic.MInstr in
  let force_register_op o = Operand.reg @@ force_register o in
  let not_both_mem o1 o2 =
    match o1, o2 with
    | Operand.Mem _, Operand.Mem _ ->
      let o2 = force_register_op o2 in
      o1, o2
    | _ -> o1, o2
  in
  let legal ~dst ~src =
    let `dst dst, `src src = force_same ~dst ~src in
    let dst, src = not_both_mem dst src in
    `dst dst, `src src
  in
  match instr with
  | Add ({ dst; src1; _ } as p) ->
    let `dst dst, `src src1 = legal ~dst ~src:src1 in
    Add { p with dst; src1 }
  | Mov ({ dst; src; _ } as p) ->
    let dst, src = not_both_mem dst src in
    Mov { p with dst; src }
  | Cmp ({ src1; src2; _ } as p) ->
    let src1, src2 = not_both_mem src1 src2 in
    Cmp { p with src1; src2 }
  | Test ({ src1; src2; _ } as p) ->
    let src1, src2 = not_both_mem src1 src2 in
    Test { p with src1; src2 }
  | _ -> todo ()
;;
