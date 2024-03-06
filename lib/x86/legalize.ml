open O
open Types

type context =
  { instrs : (AReg.t Flat.Line.t, read_write) Vec.t (* ; stack_layout : Stack_layout.t *)
  }

module Cx = struct
  let create () = { instrs = Vec.create () }
  let add cx i = Vec.push cx.instrs (Instr i)
  let add_label cx label = Vec.push cx.instrs (Label label)
  (* let add_vinstr cx vinstr = Vec.push cx.instrs (Instr.Virt vinstr)
     let add cx = Vec.push cx.instrs *)
  (* let add_minstr cx minstr = Vec.push cx.instrs minstr *)
end

let to_op = function
  | Operand.Mem mem -> Flat.Op.Mem mem
  | Operand.Reg reg -> Flat.Op.Reg reg
  | Operand.Imm imm -> Flat.Op.Imm imm
;;

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
    List.map movs ~f:(fun { dst; src } -> Flat.Instr.Mov { dst = Reg dst; src = Reg src })
  in
  movs
;;

let legalize_vinstr cx instr =
  match instr with
  | VInstr.Par_mov movs ->
    let res = legalize_par_mov movs in
    List.iter res ~f:(Cx.add cx);
    ()
  | _ -> ()
;;

let legalize_instr cx (instr : AReg.t Instr.t) =
  let open Types_basic.Instr in
  let force_register ~size o =
    let reg = AReg.InReg { s = size; name = Some "scratch"; reg = Mach_reg.R11 } in
    Cx.add cx @@ Mov { dst = Reg reg; src = to_op o };
    reg
  in
  let force_same ~dst ~src =
    let get_size = function
      | Operand.Reg r -> AReg.size r
      | Operand.Mem mem -> mem.size
      | _ -> failwith "get_size"
    in
    match dst, src with
    | Operand.Mem _, Operand.Mem _ ->
      let size = get_size dst in
      let reg = force_register ~size src in
      Cx.add cx @@ Mov { dst = to_op dst; src = Reg reg };
      Operand.Reg reg
    | _ ->
      Cx.add cx @@ Mov { dst = to_op dst; src = to_op src };
      dst
  in
  let force_register_op size o = Operand.reg @@ force_register ~size o in
  (* s is the size of o2 *)
  let legal_mem o1 o2 =
    match o1, o2 with
    | Operand.Mem _, Operand.Mem { size; _ } -> force_register_op size o2
    | _ -> o2
  in
  (* precondition: instruction must work on the same size for all three operands *)
  let legal3 dst src1 src2 =
    let dst = force_same ~dst ~src:src1 in
    let src2 = legal_mem dst src2 in
    dst, src2
  in
  (match instr with
   | Instr.Virt instr -> legalize_vinstr cx instr
   | Jump (Jump j) -> Cx.add cx @@ Jmp { src = Entity.Name.to_dotted_string j.label }
   | Jump (CondJump { cond; j1; j2 }) ->
     Cx.add cx @@ J { cond; src = Entity.Name.to_dotted_string j1.label };
     Cx.add cx @@ Jmp { src = Entity.Name.to_dotted_string j2.label };
     ()
   | Jump (Ret (Some src)) ->
     Cx.add cx @@ Mov { dst = Reg (AReg.create Q RAX); src = to_op src }
   | Jump (Ret None) -> ()
   | Add { dst; src1; src2; _ } ->
     let dst, src2 = legal3 dst src1 src2 in
     Cx.add cx @@ Flat.Instr.Add { dst = to_op dst; src = to_op src2 }
   | Mov { dst; src } ->
     let src = legal_mem dst src in
     Cx.add cx @@ Mov { dst = to_op dst; src = to_op src }
   | Cmp { src1; src2; _ } ->
     let src2 = legal_mem src1 src2 in
     Cx.add cx @@ Cmp { src1 = to_op src1; src2 = to_op src2 }
   | Test { src1; src2; _ } ->
     let src2 = legal_mem src1 src2 in
     Cx.add cx @@ Test { src1 = to_op src1; src2 = to_op src2 }
   | MovAbs { dst; imm } -> Cx.add cx @@ MovAbs { dst = to_op dst; imm }
   | Set { cond; dst } -> Cx.add cx @@ Set { cond; dst = to_op dst }
   | instr -> raise_s [%message "can't legalize instr" (instr : _ Instr.t)]);
  ()
;;

let legalize_block cx label (block : _ Block.t) =
  Cx.add_label cx (Entity.Name.to_dotted_string label);
  Block.instrs_forward_fold block |> F.Iter.iter ~f:(legalize_instr cx)
;;

let legalize_function (fn : _ Function.t) =
  let cx = Cx.create () in
  Cfg.Graph.to_iteri fn.graph (fun (label, block) -> legalize_block cx label block);
  Vec.shrink_to_fit cx.instrs;
  Vec.freeze cx.instrs
;;
(* Function.blocks_forward_fold fn |> F.Iter.iter ~f:(legalize_block cx *)

(* let legalize_function fn = Function.map_blocks fn ~f:legalize_block *)
