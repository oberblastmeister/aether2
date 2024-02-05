open O
open Types
module Tir = Lower.Tir

module X86 = struct
  include X86
  include X86.Types
end

module MInstr = X86.MInstr
module VInstr = X86.VInstr

module Context = struct
  type t =
    { instrs : (X86.VReg.t X86.Instr.t, Perms.Read_write.t) Vec.t
    ; mutable unique_name : Name.Id.t
    }

  let create (fn : Tir.Function.t) =
    { instrs = Vec.create (); unique_name = fn.unique_name }
  ;;

  let add cx instr = Vec.push cx.instrs (X86.Instr.Real instr)
  let addv cx instr = Vec.push cx.instrs (X86.Instr.Virtual instr)
  let addj cx instr = Vec.push cx.instrs (X86.Instr.Jump instr)
end

module Cx = Context

let ty_to_size = function
  | Ty.U1 -> X86.Size.L
  | Ty.U64 -> X86.Size.Q
;;

let precolored (v : Value.t) reg =
  X86.(Operand.Reg (VReg.precolored (ty_to_size v.ty) v.name reg))
;;

let cmp_op_to_cond ty op =
  match ty with
  | Ty.U1 | Ty.U64 ->
    (match op with
     | Cmp_op.Gt -> X86.Cond.A)
;;

let vreg (v : Value.t) = X86.VReg.create (ty_to_size v.ty) v.name

let fresh_value (cx : Context.t) (v : Value.t) : Value.t =
  let id = cx.unique_name in
  let name = Name.create v.name.name id in
  { v with name }
;;

let rec lower_value cx = function
  | Tir.Value.I { dst; expr } -> lower_assign cx dst expr
  | Tir.Value.V v -> vreg v

and lower_value_op cx v = X86.Operand.Reg (lower_value cx v)

and lower_assign cx dst expr =
  match expr with
  | Expr.Bin { ty; op; v1; v2 } ->
    (match op with
     | Bin_op.Add ->
       let src1 = lower_value_op cx v1 in
       let src2 = lower_value_op cx v2 in
       let dst = vreg dst in
       Cx.add cx (MInstr.Add { s = ty_to_size ty; dst = X86.Operand.Reg dst; src1; src2 });
       dst
     | _ -> failwith "can't handle op yet")
  | Expr.Const { ty = Ty.U64; const } ->
    let dst = vreg dst in
    Cx.add cx (MInstr.MovImm64 { dst = X86.Operand.Reg dst; imm = const });
    dst
  | Expr.Const { ty; const } ->
    let dst = vreg dst in
    Cx.add
      cx
      (MInstr.Mov
         { s = ty_to_size ty
         ; dst = X86.Operand.Reg dst
         ; src = X86.Operand.imm (Int64.to_int32_exn const)
         });
    dst
  | Expr.Cmp { ty; op; v1; v2 } ->
    let src1 = lower_value_op cx v1 in
    let src2 = lower_value_op cx v2 in
    Cx.add cx (MInstr.Cmp { s = ty_to_size ty; src1; src2 });
    let dst = vreg dst in
    Cx.add
      cx
      (MInstr.Set
         { s = ty_to_size ty; cond = cmp_op_to_cond ty op; dst = X86.Operand.Reg dst });
    dst
  | Expr.Val { ty; v } ->
    let dst = vreg dst in
    let src = lower_value_op cx v in
    Cx.add cx (MInstr.Mov { s = ty_to_size ty; dst = X86.Operand.Reg dst; src });
    dst
  | Expr.Alloca _ -> todo ()
  | Expr.Load _ -> todo ()
  | _ -> todo ()

and lower_instr cx instr =
  match instr with
  | Instr.Assign { dst; expr } -> lower_assign cx dst expr
  | Instr.Store _ -> todo ()

and lower_block_call cx block_call =
  { X86.Block_call.label = block_call.Block_call.label
  ; args = X86.Maybe_block_call.Block_call (List.map ~f:(lower_value cx) block_call.args)
  }

and lower_block_args cx (block_args : Block_args.t) =
  let block_args = List.map ~f:(fun v -> vreg v) block_args in
  Cx.addv cx @@ VInstr.Block_args block_args

and lower_control_instr cx instr =
  match instr with
  | Control_instr.CondJump (v, bc1, bc2) ->
    let op1 = lower_value_op cx v in
    let bc1 = lower_block_call cx bc1 in
    let bc2 = lower_block_call cx bc2 in
    Cx.add cx
    @@ MInstr.Test { s = ty_to_size @@ Tir.Value.get_ty v; src1 = op1; src2 = op1 };
    Cx.addj cx @@ X86.Jump.CondJump { cond = X86.Cond.NE; j1 = bc1; j2 = bc2 }
  | Control_instr.Jump j ->
    let j = lower_block_call cx j in
    Cx.addj cx @@ X86.Jump.Jump j
  | Ret None -> Cx.add cx @@ MInstr.Ret
  | Ret (Some v) ->
    let op = lower_value_op cx v in
    let v' = fresh_value cx (Tir.Value.to_value v) in
    let dst = precolored v' X86.Mach_reg.RAX in
    Cx.add cx @@ MInstr.Mov { s = ty_to_size @@ Tir.Value.get_ty v; dst; src = op };
    Cx.add cx @@ MInstr.Ret
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

let lower_graph cx graph =
  (Cfg.Graph.map_blocks & FC.Map.map) graph ~f:(fun block -> lower_block cx block)
;;

let lower_function (fn : Tir.Function.t) =
  let cx = Context.create fn in
  let graph = lower_graph cx fn.graph in
  { X86.Function.graph }
;;

let lower (prog : Tir.Program.t) =
  let functions = List.map ~f:lower_function prog.functions in
  { X86.Program.functions }
;;
