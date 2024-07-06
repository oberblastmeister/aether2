(* TODO
   we have two options:
   1. We can define a lower function cogen(d, e) that lowers an expression e to a destination d
   This has the benefit of avoiding a move when we are lowering a move instruction,
   because we already have the destination, so we can pass it into the lower_to_dst function.

   However, this has the downside that we need extra moves when we are lowering in an expression context,
   because we have to create a new destination and then pass it into lower_to_dst.
   For example, when lowering constant expressions we need to create a new register for the constant expression instead of just using it as an immediate

   2. We can define a lower function gen(e) that lowers an expression e and returns the operand v used to refer to it.
   This is better in the second case but worse in the first case, because we need to create an extra destination and move the returned operand to it.

   We will choose the second option, because copy propagation is easier in lowered code than constant propagation.
   This is because constant propagation actually changes the form of something (from a register to an immediate),
   while copy propagation doesn't (from a register to another register)

   For example, let's say a register r1 is used inside of an address [r1 + r2 * 8].
   Replacing r1 with a constant completely changes the addressing mode which is nontrivial.

   However, just replacing r1 with another register, say r3, is trivial, because the addressing mode is the same.
*)
open O
open Ast
module Tir = Lower.Tir

module X86 = struct
  include X86
  include X86.Ast
end

module Module_context = struct
end

module Context = struct
  type t =
    { instrs : (X86.VReg.t X86.Instr.t, Perms.Read_write.t) Vec.t
    ; mutable unique_name : Name.Id.t
    ; mutable stack_instrs : X86.Stack_instr.t list
    ; mutable unique_stack_slot : X86.Stack_slot.Id.t
    }

  let create (fn : Tir.Function.t) =
    { instrs = Vec.create ()
    ; unique_name = fn.unique_name
    ; unique_stack_slot = X86.Stack_slot.Id.of_int 0
    ; stack_instrs = []
    }
  ;;

  let add_stack_instr cx instr = cx.stack_instrs <- instr :: cx.stack_instrs
  let add cx instr = Vec.push cx.instrs instr
  let addv cx instr = Vec.push cx.instrs (X86.Instr.Virt instr)
  let addj cx instr = Vec.push cx.instrs (X86.Instr.Jump instr)
end

module Cx = Context

let ty_to_size = function
  | Ty.I1 -> X86.Size.B
  | Ty.I64 -> X86.Size.Q
  | Ty.Ptr -> X86.Size.Q
  | Void -> raise_s [%message "void has no size"]
;;

let value_size v = ty_to_size v.Value.ty

let cmp_op_to_cond signed op =
  match signed with
  | Signed.Unsigned ->
    (match op with
     | Cmp_op.Gt -> X86.Cond.A
     | Ge -> X86.Cond.AE
     | Eq -> X86.Cond.E
     | Lt -> X86.Cond.B
     | Le -> X86.Cond.BE)
  | Signed.Signed ->
    (match op with
     | Gt -> X86.Cond.G
     | Ge -> X86.Cond.GE
     | Eq -> X86.Cond.E
     | Lt -> X86.Cond.L
     | Le -> X86.Cond.LE)
;;

let fresh_name (cx : Context.t) (name : string) : Name.t =
  let id = cx.unique_name in
  cx.unique_name <- Name.Id.next cx.unique_name;
  Name.create name id
;;

let fresh_vreg cx (name : string) =
  let name = fresh_name cx name in
  X86.VReg.create name
;;

let fresh_op cx (name : string) =
  let vreg = fresh_vreg cx name in
  X86.Operand.Reg vreg
;;

let vreg (v : Value.t) = X86.VReg.create v.name

let categorize_args args =
  let registers = X86.Mach_reg.args in
  let args_with_reg, remaining = List.zip_with_remainder args registers in
  let stack_args =
    match remaining with
    | Some (First args) -> args
    | None | Some (Second _) -> []
  in
  args_with_reg, stack_args
;;

let rec lower_call cx Call.{ name; args } dst =
  let args = List.map ~f:(lower_expr_simple_op cx) args in
  let args_with_reg, stack_args = categorize_args args in
  Cx.add_stack_instr cx
  @@ X86.Stack_instr.ReserveEnd
       { size = Int32.(of_int_exn (List.length stack_args) * 8l) };
  List.iter stack_args
  |> F.Iter.enumerate
  |> F.Iter.iter ~f:(fun (i, arg) ->
    Cx.add cx
    @@ Mov
         { s = Q
         ; dst = X86.Operand.stack_off_end Int32.(of_int_exn i * 8l)
         ; src = X86.Operand.of_simple arg
         };
    ());
  Cx.add
    cx
    (Call
       { name
       ; reg_args = List.map ~f:(fun (x, y) -> y, x) args_with_reg
       ; defines = X86.Mach_reg.caller_saved_without_r11
       ; dst
       });
  ()

and lower_expr_op cx (expr : Tir.Value.t Expr.t) : X86.VReg.t X86.Operand.t =
  match expr with
  | Bin { ty = _; op; v1; v2; _ } ->
    (match op with
     | Bin_op.Add ->
       let dst = fresh_op cx "tmp" in
       let src1 = lower_expr_op cx v1 in
       let src2 = lower_expr_op cx v2 in
       Cx.add cx (Add { s = Q; dst; src1; src2 });
       dst
     | Sub ->
       let dst = fresh_op cx "tmp" in
       let src1 = lower_expr_op cx v1 in
       let src2 = lower_expr_op cx v2 in
       Cx.add cx (Sub { s = Q; dst; src1; src2 });
       dst
     | Mul ->
       let dst = fresh_op cx "tmp" in
       let src1 = lower_expr_op cx v1 in
       let src2 = lower_expr_op cx v2 in
       Cx.add cx (Imul { s = Q; dst; src1; src2 });
       dst)
  | Const { ty = Ty.I64; const } when is_some (X86.Imm_int.of_z const) ->
    X86.Operand.imm (X86.Imm_int.of_z_exn const)
  | Const { ty = Ty.I64; const } ->
    let dst = fresh_op cx "tmp" in
    Cx.add cx (MovAbs { dst; imm = const });
    dst
  | Const { ty = Ty.I1; const; _ } -> X86.Operand.imm (X86.Imm_int.of_z_exn const)
  | Const { ty = Ty.Void; _ } -> raise_s [%message "const can't be void" [%here]]
  | Cmp { ty = _; signed; op; v1; v2 } ->
    let tmp = fresh_op cx "tmp" in
    let dst = fresh_op cx "tmp" in
    let src1 = lower_expr_op cx v1 in
    let src2 = lower_expr_op cx v2 in
    Cx.add cx (Cmp { s = Q; src1; src2 });
    Cx.add cx (Set { dst = tmp; cond = cmp_op_to_cond signed op });
    Cx.add cx (MovZx { dst_size = Q; src_size = B; dst; src = tmp });
    dst
  | Val (I { expr; _ }) -> lower_expr_op cx expr
  | Val (I' { expr; _ }) -> Reg (lower_impure_expr_reg cx expr)
  | Val (V v) -> Reg (vreg v)
  | _ -> todo [%here]

and lower_expr_simple_op cx expr : _ X86.Simple_operand.t =
  match lower_expr_op cx expr with
  | Reg v -> X86.Simple_operand.Reg v
  | Imm i -> Imm i
  | op ->
    let dst = fresh_vreg cx "tmp" in
    Cx.add cx (Mov { s = Q; dst = Reg dst; src = op });
    X86.Simple_operand.Reg dst

and lower_expr_reg cx expr =
  match lower_expr_op cx expr with
  | Reg v -> v
  | op ->
    let dst = fresh_vreg cx "tmp" in
    Cx.add cx (Mov { s = Q; dst = Reg dst; src = op });
    dst

and lower_impure_expr_reg cx expr =
  match expr with
  | Impure_expr.Call { call; _ } ->
    let dst = fresh_vreg cx "tmp" in
    lower_call cx call (Some (dst, X86.Mach_reg.ret));
    dst
  | Load { ty = _; ptr } ->
    let dst = fresh_vreg cx "tmp" in
    let ptr_reg = lower_expr_reg cx ptr in
    Cx.add cx (Mov { s = Q; dst = Reg dst; src = Mem (X86.Address.reg ptr_reg) });
    dst
  | Alloca { size } ->
    let dst = fresh_vreg cx "tmp" in
    let stack_slot = X86.Stack_slot.create "alloca" cx.unique_stack_slot in
    cx.unique_stack_slot <- X86.Stack_slot.Id.next cx.unique_stack_slot;
    cx.stack_instrs
    <- X86.Stack_instr.ReserveLocal { stack_slot; size } :: cx.stack_instrs;
    Cx.add cx
    @@ Lea
         { s = Q
         ; dst
         ; src = Complex { base = Rsp; index = None; offset = Stack (Local stack_slot) }
         };
    dst
  | Udiv { ty = _; v1; v2 } ->
    let dst = fresh_vreg cx "tmp" in
    let src1 = lower_expr_op cx v1 in
    let src2 = lower_expr_op cx v2 in
    Cx.add cx (Div { s = Q; dst = Reg dst; src1; src2 });
    dst
  | Idiv { ty = _; v1; v2 } ->
    let dst = fresh_vreg cx "tmp" in
    let src1 = lower_expr_op cx v1 in
    let src2 = lower_expr_op cx v2 in
    Cx.add cx (Idiv { s = Q; dst = Reg dst; src1; src2 });
    dst

and lower_instr cx instr =
  match instr with
  | Instr.Assign { dst; expr } ->
    let op = lower_expr_op cx expr in
    Cx.add cx (Mov { s = value_size dst; dst = X86.Operand.Reg (vreg dst); src = op });
    ()
  | Instr.ImpureAssign { dst; expr } ->
    let reg = lower_impure_expr_reg cx expr in
    Cx.add
      cx
      (Mov { s = value_size dst; dst = X86.Operand.Reg (vreg dst); src = Reg reg });
    ()
  | VoidCall call -> lower_call cx call None
  | Store { ty = _; ptr; expr } ->
    let ptr = lower_expr_reg cx ptr in
    let expr = lower_expr_op cx expr in
    Cx.add cx (Mov { s = Q; dst = Mem (X86.Address.reg ptr); src = expr });
    ()

and lower_block_call cx block_call =
  { X86.Block_call.label = block_call.Block_call.label
  ; args = List.map ~f:(lower_expr_simple_op cx) block_call.args
  }

and lower_block_args cx (block_args : Block_args.t) =
  let block_args = List.map ~f:(fun v -> vreg v) block_args in
  Cx.addv cx @@ Block_args block_args

and lower_control_instr cx instr =
  match instr with
  | Control_instr.CondJump (v, bc1, bc2) ->
    let op1 = lower_expr_op cx v in
    let bc1 = lower_block_call cx bc1 in
    let bc2 = lower_block_call cx bc2 in
    Cx.add cx @@ Test { s = B; src1 = op1; src2 = op1 };
    Cx.addj cx @@ X86.Jump.CondJump { cond = X86.Cond.NE; j1 = bc1; j2 = bc2 }
  | Control_instr.Jump j ->
    let j = lower_block_call cx j in
    Cx.addj cx @@ X86.Jump.Jump j
  | Ret v ->
    let v = Option.map ~f:(fun expr -> X86.Size.Q, lower_expr_op cx expr) v in
    Cx.addj cx @@ X86.Jump.Ret v
;;

let lower_block cx (block : Tir.Block.t) =
  lower_block_args cx block.entry;
  List.iter block.body ~f:(fun instr ->
    let _ = lower_instr cx instr in
    ());
  lower_control_instr cx block.exit;
  let instrs = cx.instrs |> Vec.copy_exact |> Vec.freeze in
  Vec.clear cx.instrs;
  { X86.Block.instrs }
;;

let lower_graph cx graph = Cfg.Graph.map graph ~f:(fun block -> lower_block cx block)

let lower_function (fn : Tir.Function.t) =
  let cx = Context.create fn in
  let params, stack_params = categorize_args fn.ty.params in
  let params =
    List.map params ~f:(fun (value, mach_reg) -> vreg value, X86.MReg.create mach_reg)
  in
  let stack_params = List.map stack_params ~f:vreg in
  let graph = lower_graph cx fn.graph in
  { name = fn.name
  ; X86.Function.graph
  ; caller_saved = X86.Mach_reg.caller_saved (* ; params = *)
  ; params
  ; stack_params
  ; unique_name = cx.unique_name
  ; unique_stack_slot = cx.unique_stack_slot
  ; stack_instrs = cx.stack_instrs
  }
;;

let lower_decl decl =
  match decl with
  | Decl.Func func -> [ lower_function func ]
  | _ -> []
;;

let lower (modul : Tir.Module.t) =
  let functions =
    F.Iter.to_list (Module.iter_decls modul) |> List.concat_map ~f:lower_decl
  in
  { X86.Program.functions }
;;
