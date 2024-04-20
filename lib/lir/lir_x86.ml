open O
open Ast
module Tir = Lower.Tir

module X86 = struct
  include X86
  include X86.Ast
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

  let add cx instr = Vec.push cx.instrs instr
  let addv cx instr = Vec.push cx.instrs (X86.Instr.Virt instr)
  let addj cx instr = Vec.push cx.instrs (X86.Instr.Jump instr)
end

module Cx = Context

let ty_to_size = function
  | Ty.U1 -> X86.Size.B
  | Ty.U64 -> X86.Size.Q
  | Void -> raise_s [%message "void has no size"]
;;

let value_size v = ty_to_size v.Value.ty

let cmp_op_to_cond ty op =
  match ty with
  | Ty.U1 | Ty.U64 ->
    (match op with
     | Cmp_op.Gt -> X86.Cond.A
     | Ge -> X86.Cond.AE
     | Eq -> X86.Cond.E)
  | Void -> raise_s [%message "cannot use void with cond"]
;;

let fresh_name (cx : Context.t) (name : string) : Name.t =
  let id = cx.unique_name in
  cx.unique_name <- Name.Id.next cx.unique_name;
  Name.create name id
;;

let fresh_value (cx : Context.t) name ty =
  let name = fresh_name cx name in
  { Value.name; ty }
;;

let freshen_name (cx : Context.t) (name : Name.t) : Name.t =
  let id = cx.unique_name in
  cx.unique_name <- Name.Id.next cx.unique_name;
  Name.create name.name id
;;

let vreg (v : Value.t) = X86.VReg.create (ty_to_size v.ty) v.name

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

let rec lower_value cx = function
  | Tir.Value.I { dst; expr } ->
    lower_expr_to cx dst expr;
    dst
  | Tir.Value.I' { dst; expr } ->
    lower_impure_expr_to cx dst expr;
    dst
  | Tir.Value.V v -> v

and lower_value_reg cx v = vreg (lower_value cx v)
and lower_value_op cx v = X86.Operand.Reg (vreg (lower_value cx v))

and lower_value_op_merge cx (v : Tir.Value.t) : _ X86.Operand.t =
  match v with
  | I { expr = Const { ty = U64; const }; _ }
    when Int64.(const <= Int64.of_int32 Int32.max_value) ->
    Imm (Int (Int32.of_int64_exn const))
  | I { dst; expr = Const { ty = U64; const }; _ } ->
    let dst = vreg dst in
    Cx.add cx (MovAbs { dst = Reg dst; imm = const });
    Reg dst
  | I { dst = _; expr = Val v; _ } -> lower_value_op_merge cx v
  (* we can't merge it *)
  | v -> lower_value_op cx v

and lower_call cx Call.{ name; args } dst =
  let args = List.map ~f:(lower_expr cx) args in
  let args_with_reg, stack_args = categorize_args args in
  List.iter stack_args
  |> F.Iter.enumerate
  |> F.Iter.iter ~f:(fun (i, arg) ->
    Cx.add cx
    @@ Mov { dst = X86.Operand.stack_off_end Q Int32.(of_int_exn i * 8l); src = Reg arg };
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

and lower_expr_to cx dst (expr : Tir.Value.t Expr.t) =
  let dst = X86.Operand.Reg (vreg dst) in
  match expr with
  | Bin { op; v1; v2; _ } ->
    (match op with
     | Bin_op.Add ->
       let src1 = lower_expr_op cx v1 in
       let src2 = lower_expr_op cx v2 in
       Cx.add cx (Add { dst; src1; src2 })
     | Sub ->
       let src1 = lower_expr_op cx v1 in
       let src2 = lower_expr_op cx v2 in
       Cx.add cx (Sub { dst; src1; src2 }))
  | Const { ty = Ty.U64; const } -> Cx.add cx (MovAbs { dst; imm = const })
  | Const { const; _ } ->
    Cx.add cx (Mov { dst; src = X86.Operand.imm (Int64.to_int32_exn const) })
  | Cmp { ty; op; v1; v2 } ->
    let src1 = lower_expr_op cx v1 in
    let src2 = lower_expr_op cx v2 in
    Cx.add cx (Cmp { src1; src2 });
    Cx.add cx (Set { dst; cond = cmp_op_to_cond ty op })
  | Val v ->
    let src = lower_value_op_merge cx v in
    Cx.add cx (Mov { dst; src })

and lower_expr cx (expr : Tir.Value.t Expr.t) : X86.VReg.t =
  let ty = Expr.get_ty_with Tir.Value.get_ty expr in
  let dst = fresh_value cx "expr_tmp" ty in
  lower_expr_to cx dst expr;
  vreg dst

and lower_impure_expr_to cx dst expr =
  match expr with
  | Impure_expr.Call { call; _ } ->
    lower_call cx call (Some (vreg dst, X86.Mach_reg.ret));
    ()
  | Load _ -> todo [%here]
  | Alloca _ -> todo [%here]

and lower_expr_op cx expr =
  match expr with
  | Expr.Const { ty = Ty.U64; const }
    when Int64.( <= ) const (Int64.of_int32 Int32.max_value) ->
    X86.Operand.Imm (Int (Int32.of_int64_exn const))
  | _ -> X86.Operand.Reg (lower_expr cx expr)

and lower_instr cx instr =
  match instr with
  | Instr.Assign { dst; expr } -> lower_expr_to cx dst expr
  | Instr.ImpureAssign { dst; expr } -> lower_impure_expr_to cx dst expr
  | VoidCall call -> lower_call cx call None
  | Store _ -> todo [%here]

and lower_block_call cx block_call =
  { X86.Block_call.label = block_call.Block_call.label
  ; args = List.map ~f:(lower_expr cx) block_call.args
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
    Cx.add cx @@ Test { src1 = op1; src2 = op1 };
    Cx.addj cx @@ X86.Jump.CondJump { cond = X86.Cond.NE; j1 = bc1; j2 = bc2 }
  | Control_instr.Jump j ->
    let j = lower_block_call cx j in
    Cx.addj cx @@ X86.Jump.Jump j
  | Ret v ->
    let v = Option.map ~f:(lower_expr_op cx) v in
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
  let graph = lower_graph cx fn.graph in
  let params, stack_params = categorize_args fn.ty.params in
  let params =
    List.map params ~f:(fun (value, mach_reg) ->
      vreg value, X86.MReg.create (value_size value) mach_reg)
  in
  let stack_params = List.map stack_params ~f:vreg in
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

let lower (prog : Tir.Program.t) =
  let functions = List.map ~f:lower_function prog.funcs in
  { X86.Program.functions }
;;
