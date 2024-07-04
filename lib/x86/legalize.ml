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

let legalize_par_mov' movs =
  let module W = Compiler.Windmills in
  let movs =
    List.map movs ~f:(fun (dst, src) ->
      W.Move.create ~dst:(Simple_operand.Reg dst) ~src ~ann:())
  in
  [%log.global.debug "movs before" (movs : (unit, AReg.t Simple_operand.t) W.Move.t list)];
  let movs, _did_use_scratch =
    W.convert
      ~eq:(fun op1 op2 ->
        match op1, op2 with
        | Simple_operand.Imm _, _ -> false
        | _, Simple_operand.Imm _ -> false
        | Simple_operand.Reg r1, Simple_operand.Reg r2 -> AReg.equal r1 r2)
      ~scratch:(fun _reg ->
        Simple_operand.Reg (AReg.InReg { name = Some "par_mov_scratch"; reg = R11 }))
      movs
  in
  [%log.global.debug "movs after" (movs : (unit, AReg.t Simple_operand.t) W.Move.t list)];
  let movs =
    List.map movs ~f:(fun { dst; src; ann = () } ->
      Flat.Instr.Mov { s = Q; dst = Operand.of_simple dst; src = Operand.of_simple src })
  in
  movs
;;

let legalize_par_mov cx movs = List.iter ~f:(Cx.add cx) (legalize_par_mov' movs)

let legalize_vinstr cx instr =
  match instr with
  | VInstr.Par_mov movs ->
    let res = legalize_par_mov' movs in
    List.iter res ~f:(Cx.add cx);
    ()
  | _ -> ()
;;

let legalize_instr cx (instr : AReg.t Instr.t) =
  let open Ast.Instr in
  let force_register ~size o =
    let reg = AReg.InReg (MReg.create ~name:"scratch" R11) in
    Cx.add cx @@ Mov { s = size; dst = Reg reg; src = to_op o };
    reg
  in
  let force_register_op size o = Operand.reg @@ force_register ~size o in
  (* for instructions like test and cmp *)
  let legal2 s o1 o2 =
    match o1, o2 with
    (* memory can never be on the right *)
    | _, Operand.Mem _ -> o1, force_register_op s o2
    (* imm can never be on the left *)
    | Operand.Imm _, _ -> force_register_op s o1, o2
    | _ -> o1, o2
  in
  let legal_mov s dst src =
    match dst, src with
    | Operand.Mem _, Operand.Mem _ -> force_register_op s src
    | Operand.Imm _, _ ->
      raise_s [%message "dst can't be imm" (dst : _ Operand.t) [%here]]
    | _ -> src
  in
  (* for 3 operand read-modify-write instructions *)
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
     let src = legal_mov s dst src in
     Cx.add cx @@ Mov { s; dst; src }
   | Cmp { s; src1; src2; _ } ->
     let src1, src2 = legal2 s src1 src2 in
     Cx.add cx @@ Cmp { s; src1; src2 }
   | Test { s; src1; src2; _ } ->
     let src1, src2 = legal2 s src1 src2 in
     Cx.add cx @@ Test { s; src1; src2 }
   | MovAbs { dst; imm } -> Cx.add cx @@ MovAbs { dst; imm }
   | MovZx { dst_size; src_size; dst; src } ->
     let src = legal_mov src_size dst src in
     Cx.add cx @@ MovZx { dst_size; src_size; dst; src };
     ()
   | Set { cond; dst } -> Cx.add cx @@ Set { cond; dst }
   | Call { name; reg_args; dst; _ } ->
     List.map reg_args ~f:(fun (mach_reg, reg) -> AReg.create mach_reg, reg)
     |> legalize_par_mov cx;
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
     Cx.add cx @@ Mov { s; dst = Reg (AReg.create R11); src = src2 };
     Cx.add cx @@ Mov { s; dst = Reg (AReg.create RAX); src = src1 };
     Cx.add cx
     @@ Mov
          { s
          ; dst = Reg (AReg.create Mach_reg.RDX)
          ; src = Imm (Int (Imm_int.of_int32 0l))
          };
     Cx.add cx @@ Div { s; src = Reg (AReg.create R11) };
     Cx.add cx @@ Mov { s; dst; src = Reg (AReg.create Mach_reg.RAX) };
     ()
   | Idiv { s; dst; src1; src2 } ->
     Cx.add cx @@ Mov { s; dst = Reg (AReg.create R11); src = src2 };
     Cx.add cx @@ Mov { s; dst = Reg (AReg.create RAX); src = src1 };
     Cx.add cx @@ Cqto;
     Cx.add cx @@ Idiv { s; src = Reg (AReg.create R11) };
     Cx.add cx @@ Mov { s; dst; src = Reg (AReg.create Mach_reg.RAX) };
     ()
   | Lea { s; dst; src } -> Cx.add cx @@ Lea { s; dst = Reg dst; src = Mem src }
   | NoOp -> ()
   | Push _ | Pop _ ->
     raise_s [%message "can't legalize instr" (instr : _ Instr.t) [%here]]);
  ()
;;

let legalize_block cx label (block : _ Block.t) =
  Cx.add_label cx label;
  Block.iter_instrs_forward block |> F.Iter.iter ~f:(legalize_instr cx)
;;

let legalize_function ~func_index (fn : _ Function.t) =
  let cx = Cx.create func_index in
  legalize_par_mov
    cx
    (List.map
       ~f:(fun (vreg, reg) -> vreg, Simple_operand.Reg (AReg.of_mreg reg))
       fn.params);
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
