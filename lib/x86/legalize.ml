open O
open Types

type context =
  { instrs : (AReg.t Instr.t, read_write) Vec.t (* ; stack_layout : Stack_layout.t *) }

module Cx = struct
  let create () = { instrs = Vec.create () }
  let add cx = Vec.push cx.instrs
  let add_vinstr cx vinstr = Vec.push cx.instrs (Instr.Virt vinstr)
  let add cx = Vec.push cx.instrs
  let add_minstr cx minstr = Vec.push cx.instrs (Instr.Real minstr)
end

let legalize_par_mov movs =
  let module W = Compiler.Windmills in
  let movs = List.map movs ~f:(fun (dst, src) -> W.Move.create ~dst ~src) in
  let movs, _did_use_scratch =
    W.convert
      ~eq:[%equal: AReg.t]
      ~scratch:(fun reg ->
        AReg.InReg { s = AReg.size reg; name = Some "par_mov_scratch"; reg = R11 })
      movs
  in
  let movs =
    List.map movs ~f:(fun { dst; src } ->
      MInstr.Mov { s = AReg.size dst; dst = Reg dst; src = Reg src })
  in
  movs
;;

let legalize_minstr cx (instr : AReg.t MInstr.t) =
  let open Types_basic.MInstr in
  let force_register ~size o =
    let reg = AReg.InReg { s = size; name = Some "scratch"; reg = Mach_reg.R11 } in
    Cx.add_minstr cx @@ Mov { s = size; dst = Reg reg; src = o };
    reg
  in
  let force_same ~size ~dst ~src =
    match dst, src with
    | Operand.Mem _, Operand.Mem _ ->
      let reg = force_register ~size src in
      Cx.add_minstr cx @@ Mov { s = size; dst; src = Reg reg };
      Operand.Reg reg
    | _ ->
      Cx.add_minstr cx @@ Mov { s = size; dst; src };
      dst
  in
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
  let instr =
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
  in
  Cx.add_minstr cx instr;
  ()
;;

let legalize_vinstr cx instr =
  match instr with
  | VInstr.Par_mov movs ->
    let res = legalize_par_mov movs in
    List.iter res ~f:(Cx.add_minstr cx);
    ()
  (* keep these to calculate the stack layout *)
  | ReserveStackLocal { name; size } ->
    Cx.add_vinstr cx @@ ReserveStackLocal { name; size }
  | ReserveStackEnd { size } -> Cx.add_vinstr cx @@ ReserveStackEnd { size }
  (* don't need these anymore *)
  (* Block_args should be turned into Par_mov by remove_ssa *)
  | Block_args _ -> ()
;;

let legalize_instr cx instr =
  match instr with
  | Instr.Real instr -> legalize_minstr cx instr
  | Virt instr -> legalize_vinstr cx instr
  | Jump _ -> Cx.add cx instr
;;

let legalize_block (block : _ Block.t) =
  let cx = Cx.create () in
  Block.instrs_forward_fold block |> F.Iter.iter ~f:(legalize_instr cx);
  Vec.shrink_to_fit cx.instrs;
  { block with instrs = Vec.freeze cx.instrs }
;;

let legalize_function fn = Function.map_blocks fn ~f:legalize_block
