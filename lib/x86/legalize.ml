open O
open Ast

module Cx : sig
  type t

  val create : int -> t
  val add : t -> AReg.t Flat.Instr.t -> unit
  val add_label : t -> Label.t -> unit
  val string_of_label : t -> Label.t -> string

  (* Cx should not be used afterwards *)
  val get_instrs : t -> (AReg.t Flat.Line.t, read) Vec.t
end = struct
  type t =
    { instrs :
        (AReg.t Flat.Line.t, read_write) Vec.t (* ; stack_layout : Stack_layout.t *)
    ; func_index : int
    }

  let create func_index = { instrs = Vec.create (); func_index }
  let add cx i = Vec.push cx.instrs (Instr i)

  let string_of_label cx (label : Label.t) =
    [%string
      ".L%{string_of_int cx.func_index}%{string_of_int (Label.Id.to_int label.id)}"]
  ;;

  let add_label cx (label : Label.t) =
    Vec.push cx.instrs (Comment [%string "label: %{label.name}"]);
    Vec.push cx.instrs (Label (string_of_label cx label))
  ;;

  let get_instrs cx = Vec.freeze cx.instrs
end

let to_op = function
  | Operand.Mem mem -> Flat.Op.Mem mem
  | Operand.Reg reg -> Flat.Op.Reg reg
  | Operand.Imm imm -> Flat.Op.Imm imm
;;

let legalize_par_mov movs =
  let module W = Compiler.Windmills in
  let movs = List.map movs ~f:(fun (dst, src) -> W.Move.create ~dst ~src ~ann:()) in
  [%log.global.debug "movs before" (movs : (unit, AReg.t) W.Move.t list)];
  let movs, _did_use_scratch =
    W.convert
      ~eq:[%equal: AReg.t]
      ~scratch:(fun reg ->
        AReg.InReg
          { name = Some "par_mov_scratch"
          ; reg = Reg_class.scratch_reg_of_class (AReg.reg_class reg)
          })
      movs
  in
  [%log.global.debug "movs after" (movs : (unit, AReg.t) W.Move.t list)];
  let movs =
    List.map movs ~f:(fun { dst; src; ann = () } ->
      Flat.Instr.Mov
        { s = Reg_class.max_size (AReg.reg_class dst); dst = Reg dst; src = Reg src })
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
  let open Ast.Instr in
  let force_register ~size o =
    let reg = AReg.InReg { name = Some "scratch"; reg = Mach_reg.R11 } in
    Cx.add cx @@ Mov { s = size; dst = Reg reg; src = to_op o };
    reg
  in
  let force_register_op size o = Operand.reg @@ force_register ~size o in
  (* s is the size of o2 *)
  let legal_mem s o1 o2 =
    match o1, o2 with
    | Operand.Mem _, Operand.Mem _ -> force_register_op s o2
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
  let legal3 s dst src1 src2 rmw =
    let r11 = Operand.Reg (AReg.create Mach_reg.R11) in
    Cx.add cx @@ Mov { s; dst = r11; src = src1 };
    Cx.add cx @@ rmw ~s ~dst:(Operand.Reg (AReg.create Mach_reg.R11)) ~src:src2;
    Cx.add cx @@ Mov { s; dst; src = r11 }
  in
  let flip_src src1 src2 =
    match src1, src2 with
    | Operand.Imm _, _ -> src2, src1
    | _ -> src1, src2
  in
  (match instr with
   | Instr.Virt instr -> legalize_vinstr cx instr
   | Jump (Jump j) -> Cx.add cx @@ Jmp { src = Cx.string_of_label cx j.label }
   | Jump (CondJump { cond; j1; j2 }) ->
     Cx.add cx @@ J { cond; src = Cx.string_of_label cx j1.label };
     Cx.add cx @@ Jmp { src = Cx.string_of_label cx j2.label };
     ()
   | Jump (Ret src) ->
     (match src with
      | Some (s, src) -> Cx.add cx @@ Mov { s; dst = Reg (AReg.create RAX); src }
      | None -> ());
     ()
   | Add { dst; src1; src2; s } -> legal3 s dst src1 src2 Flat.Instr.add
   | Imul { dst; src1; src2; s } -> legal3 s dst src1 src2 Flat.Instr.imul
   | Sub { dst; src1; src2; s } -> legal3 s dst src1 src2 Flat.Instr.sub
   | Mov { s; dst; src } ->
     let src = legal_mem s dst src in
     Cx.add cx @@ Mov { s; dst; src }
   | Cmp { s; src1; src2; _ } ->
     let src2 = legal_mem s src1 src2 in
     let src1, src2 = flip_src src1 src2 in
     Cx.add cx @@ Cmp { s; src1; src2 }
   | Test { s; src1; src2; _ } ->
     let src2 = legal_mem s src1 src2 in
     let src1, src2 = flip_src src1 src2 in
     Cx.add cx @@ Test { s; src1; src2 }
   | MovAbs { dst; imm } -> Cx.add cx @@ MovAbs { dst; imm }
   | MovZx { dst_size; src_size; dst; src } ->
     let src = legal_mem src_size dst src in
     Cx.add cx @@ MovZx { dst_size; src_size; dst; src };
     ()
   | Set { cond; dst } -> Cx.add cx @@ Set { cond; dst }
   | Call { name; reg_args; dst; _ } ->
     List.map reg_args ~f:(fun (mach_reg, reg) -> AReg.create mach_reg, reg)
     |> legalize_par_mov
     |> List.iter ~f:(Cx.add cx);
     Cx.add cx @@ Call { src = name };
     (match dst with
      | Some (dst, dst_reg) ->
        Cx.add cx
        @@ Mov
             { s = Reg_class.max_size (Reg_class.of_mach_reg dst_reg)
             ; dst = Reg dst
             ; src = Reg (AReg.create dst_reg)
             }
      | None -> ());
     ()
   | Div { s; dst; src1; src2 } ->
    (* TODO: this is wrong, need to use legalize_par_mov here, in case src2 is allocated as rax *)
     Cx.add cx @@ Mov { s; dst = Reg (AReg.create Mach_reg.RAX); src = src1 };
     Cx.add cx
     @@ Mov
          { s
          ; dst = Reg (AReg.create Mach_reg.RDX)
          ; src = Imm (Int (Imm_int.of_int32 0l))
          };
     Cx.add cx @@ Div { s; src = src2 };
     Cx.add cx @@ Mov { s; dst; src = Reg (AReg.create Mach_reg.RAX) };
     ()
   | Idiv { s; dst; src1; src2 } ->
     Cx.add cx @@ Mov { s; dst = Reg (AReg.create Mach_reg.RAX); src = src1 };
     Cx.add cx @@ Cqto;
     Cx.add cx @@ Idiv { s; src = src2 };
     Cx.add cx @@ Mov { s; dst; src = Reg (AReg.create Mach_reg.RAX) };
     ()
   | NoOp | Lea _ | Div _ | Idiv _ | Push _ | Pop _ ->
     raise_s [%message "can't legalize instr" (instr : _ Instr.t) [%here]]);
  ()
;;

let legalize_block cx label (block : _ Block.t) =
  Cx.add_label cx label;
  Block.iter_instrs_forward block |> F.Iter.iter ~f:(legalize_instr cx)
;;

let legalize_function ~func_index (fn : _ Function.t) =
  let cx = Cx.create func_index in
  let param_movs =
    legalize_par_mov (List.map ~f:(fun (vreg, reg) -> vreg, AReg.of_mreg reg) fn.params)
  in
  List.iter param_movs ~f:(Cx.add cx);
  (* TODO: fix this shit, this should be turned into the stack ends *)
  let rsp = AReg.create Mach_reg.RSP in
  List.iter fn.stack_params
  |> F.Iter.enumerate
  |> F.Iter.iter ~f:(fun (i, param) ->
    Cx.add cx
    @@ Flat.Instr.Mov
         { s = Q
         ; dst = Reg param
         ; src =
             Mem
               (Complex
                  { base = Reg rsp
                  ; index = None
                  ; offset = Stack (Start Int32.(8l * of_int_exn i))
                  })
         });
  (* we need the first and last block to be at the end so we can patch it with a prologue and epilogue *)
  Graph.Dfs.iteri_reverse_postorder_start_end fn.graph
  |> F.Iter.iter ~f:(fun (label, block) -> legalize_block cx label block);
  Cx.get_instrs cx
;;
