open O
open Ast

type context =
  { instrs : (AReg.t Flat.Line.t, read_write) Vec.t (* ; stack_layout : Stack_layout.t *)
  }

module Cx = struct
  let create () = { instrs = Vec.create () }
  let add cx i = Vec.push cx.instrs (Instr i)
  let add_label cx label = Vec.push cx.instrs (Label label)
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

let string_of_label label = ".L" ^ Entity.Name.to_dotted_string label

let legalize_instr cx (instr : AReg.t Instr.t) =
  let open Ast_types.Instr in
  let force_register ~size o =
    let reg = AReg.InReg { s = size; name = Some "scratch"; reg = Mach_reg.R11 } in
    Cx.add cx @@ Mov { dst = Reg reg; src = to_op o };
    reg
  in
  let operand_size = function
    | Operand.Reg r -> AReg.size r
    | Operand.Mem mem -> mem.size
    | _ -> failwith "get_size"
  in
  let force_same ~dst ~src =
    match dst, src with
    | Operand.Mem _, Operand.Mem _ ->
      let size = operand_size dst in
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
  (* we have to use a scratch register here
     could try to implement it like this:

     dst <- src1
     add dst, src2

     however, if dst and src2 are allocated to the same register, then this wouldn't work.
     So we have to check that moving to dst will not affect sources.
     Furthermore, it is not clear how this combines with memory operands.
     So we just use a scratch register.
     We can then simplify all this stuff in the peephole optimization phase.
  *)
  let legal3 dst src1 src2 rmw =
    let size = operand_size dst in
    let r11 = Operand.Reg (AReg.create size Mach_reg.R11) in
    Cx.add cx @@ Mov { dst = r11; src = src1 };
    Cx.add cx @@ rmw ~dst:(Operand.Reg (AReg.create size Mach_reg.R11)) ~src:src2;
    Cx.add cx @@ Mov { dst; src = r11 }
  in
  let flip_src src1 src2 =
    match src1, src2 with
    | Operand.Imm _, _ -> src2, src1
    | _ -> src1, src2
  in
  (match instr with
   | Instr.Virt instr -> legalize_vinstr cx instr
   | Jump (Jump j) -> Cx.add cx @@ Jmp { src = string_of_label j.label }
   | Jump (CondJump { cond; j1; j2 }) ->
     Cx.add cx @@ J { cond; src = string_of_label j1.label };
     Cx.add cx @@ Jmp { src = string_of_label j2.label };
     ()
   | Jump (Ret (Some src)) -> Cx.add cx @@ Mov { dst = Reg (AReg.create Q RAX); src }
   | Jump (Ret None) -> ()
   | Add { dst; src1; src2; _ } -> legal3 dst src1 src2 Flat.Instr.add
   | Mov { dst; src } ->
     let src = legal_mem dst src in
     Cx.add cx @@ Mov { dst; src }
   | Cmp { src1; src2; _ } ->
     let src2 = legal_mem src1 src2 in
     let src1, src2 = flip_src src1 src2 in
     Cx.add cx @@ Cmp { src1; src2 }
   | Test { src1; src2; _ } ->
     let src2 = legal_mem src1 src2 in
     let src1, src2 = flip_src src1 src2 in
     Cx.add cx @@ Test { src1; src2 }
   | MovAbs { dst; imm } -> Cx.add cx @@ MovAbs { dst; imm }
   | Set { cond; dst } -> Cx.add cx @@ Set { cond; dst }
   | Call { name; reg_args; dst; _ } ->
     List.map reg_args ~f:(fun (mach_reg, reg) -> AReg.create Q mach_reg, reg)
     |> legalize_par_mov
     |> List.iter ~f:(Cx.add cx);
     (* TODO: not doing this correctly, need to par move arguments *)
     Cx.add cx @@ Call { src = name };
     (match dst with
      | Some (dst, dst_reg) ->
        Cx.add cx @@ Mov { dst = Reg dst; src = Reg (AReg.create Q dst_reg) }
      | None -> ());
     ()
   | instr -> raise_s [%message "can't legalize instr" (instr : _ Instr.t) [%here]]);
  ()
;;

let legalize_block cx label (block : _ Block.t) =
  Cx.add_label cx (string_of_label label);
  Block.iter_instrs_forward block |> F.Iter.iter ~f:(legalize_instr cx)
;;

let legalize_function (fn : _ Function.t) =
  let cx = Cx.create () in
  let param_movs =
    legalize_par_mov
      ((List.map & Tuple2.map_snd) ~f:(fun reg -> AReg.of_mreg reg) fn.params)
  in
  List.iter param_movs ~f:(Cx.add cx);
  let rsp = AReg.create Q Mach_reg.RBP in
  List.iter fn.stack_params
  |> F.Iter.enumerate
  |> F.Iter.iter ~f:(fun (i, param) ->
    Cx.add cx
    @@ Flat.Instr.Mov
         { dst =
             Mem
               { size = Q
               ; addr =
                   Complex
                     { base = Reg rsp
                     ; index = None
                     ; offset = Stack (Start Int32.(8l * of_int_exn i))
                     }
               }
         ; src = Reg param
         });
  Cfg.Graph.iter_on_labels
    (Graph.Dfs.reverse_postorder fn.graph)
    fn.graph
    ~f:(fun (label, block) -> legalize_block cx label block);
  Vec.shrink_to_fit cx.instrs;
  Vec.freeze cx.instrs
;;
