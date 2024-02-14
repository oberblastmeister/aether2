open O
open Types

let legalize_minstr instr ~force_same ~force_register =
  let open Types_basic.MInstr in
  let force_register_op size o = Operand.reg @@ force_register ~size o in
  (* s is the size of o2 *)
  let legal_mem s o1 o2 =
    match o1, o2 with
    | Operand.Mem _, Operand.Mem _ -> force_register_op s o2
    | _ -> o2
  in
  (* precondition: instruction must work on the same size for all three operands *)
  let legal3 size dst src1 src2 =
    let dst = force_same ~size ~dst ~src:src1 in
    let src2 = legal_mem size dst src2 in
    dst, dst, src2
  in
  match instr with
  | Add ({ s; dst; src1; src2; _ } as p) ->
    let dst, src1, src2 = legal3 s dst src1 src2 in
    Add { p with dst; src1; src2 }
  | Mov ({ s; dst; src } as p) ->
    let src = legal_mem s dst src in
    Mov { p with src }
  | Cmp ({ s; src1; src2; _ } as p) ->
    let src2 = legal_mem s src1 src2 in
    Cmp { p with src1; src2 }
  | Test ({ s; src1; src2; _ } as p) ->
    let src2 = legal_mem s src1 src2 in
    Test { p with src1; src2 }
  | MovAbs _ | Set _ -> instr
  | instr -> raise_s [%message "can't legalize instr" (instr : _ MInstr.t)]
;;
