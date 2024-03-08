open O
open Types
module Tir = Lower.Tir

module X86 = struct
  include X86
  include X86.Types
end

module Context = struct
  type t =
    { instrs : (X86.VReg.t X86.Instr.t, Perms.Read_write.t) Vec.t
    ; mutable unique_name : Name.Id.t
    ; mutable stack_instrs : X86.Stack_instr.t list
    }

  let create (fn : Tir.Function.t) =
    { instrs = Vec.create (); unique_name = fn.unique_name; stack_instrs = [] }
  ;;

  let add cx instr = Vec.push cx.instrs instr
  let addv cx instr = Vec.push cx.instrs (X86.Instr.Virt instr)
  let addj cx instr = Vec.push cx.instrs (X86.Instr.Jump instr)
end

module Cx = Context

let ty_to_size = function
  | Ty.U1 -> X86.Size.Q
  | Ty.U64 -> X86.Size.Q
;;

let value_size v = ty_to_size v.Value.ty

let cmp_op_to_cond ty op =
  match ty with
  | Ty.U1 | Ty.U64 ->
    (match op with
     | Cmp_op.Gt -> X86.Cond.A)
;;

let vreg (v : Value.t) = X86.VReg.create (ty_to_size v.ty) v.name

let freshen_name (cx : Context.t) (name : Name.t) : Name.t =
  let id = cx.unique_name in
  cx.unique_name <- Name.Id.next cx.unique_name;
  Name.create name.name id
;;

let fresh_value (cx : Context.t) (v : Value.t) : Value.t =
  let name = freshen_name cx v.name in
  { v with name }
;;

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
    lower_assign cx dst expr;
    dst
  | Tir.Value.V v -> v

and lower_value_reg cx v = vreg (lower_value cx v)
and lower_value_op cx v = X86.Operand.Reg (vreg (lower_value cx v))

and lower_assign cx dst (expr : _ Expr.t) : unit =
  match expr with
  | Bin { ty; op; v1; v2 } ->
    (match op with
     | Bin_op.Add ->
       let src1 = lower_value_op cx v1 in
       let src2 = lower_value_op cx v2 in
       let dst = vreg dst in
       Cx.add cx (Add { dst = X86.Operand.Reg dst; src1; src2 })
     | _ -> failwith "can't handle op yet")
  | Const { ty = Ty.U64; const } ->
    let dst = vreg dst in
    Cx.add cx (MovAbs { dst = X86.Operand.Reg dst; imm = const })
  | Const { ty; const } ->
    let dst = vreg dst in
    Cx.add
      cx
      (Mov { dst = X86.Operand.Reg dst; src = X86.Operand.imm (Int64.to_int32_exn const) })
  | Cmp { ty; op; v1; v2 } ->
    let src1 = lower_value_op cx v1 in
    let src2 = lower_value_op cx v2 in
    Cx.add cx (Cmp { src1; src2 });
    let dst = vreg dst in
    Cx.add cx (Set { cond = cmp_op_to_cond ty op; dst = X86.Operand.Reg dst })
  | Val { ty; v } ->
    let dst = vreg dst in
    let src = lower_value_op cx v in
    Cx.add cx (Mov { dst = X86.Operand.Reg dst; src })
  | Call { ty; name; args } ->
    let args = List.map ~f:(lower_value cx) args in
    let args_with_reg, stack_args = categorize_args args in
    (* List.iter args_with_reg ~f:(fun (arg, reg) ->
      let dst = precolored (fresh_value cx arg) reg in
      Cx.add cx (MInstr.Mov { s = ty_to_size arg.ty; dst; src = Reg (vreg arg) });
      ()); *)
    List.iter stack_args
    |> F.Iter.enumerate
    |> F.Iter.iter ~f:(fun (i, arg) ->
      Cx.add cx
      @@ Mov
           { dst = X86.Operand.stack_off_end Int32.(of_int_exn i * 8l)
           ; src = Reg (vreg arg)
           };
      ());
    Cx.add
      cx
      (Call
         { name
         ; reg_args = List.map ~f:(fun (x, y) -> y, vreg x) args_with_reg
         ; defines = X86.Mach_reg.caller_saved_without_r11
         ; dst_reg = X86.Mach_reg.ret
         ; dst = vreg dst
         });
    ()
  | Alloca _ -> todo ()
  | Load _ -> todo ()

and lower_instr cx instr =
  match instr with
  | Instr.Assign { dst; expr } -> lower_assign cx dst expr
  | Store _ -> todo ()

and lower_block_call cx block_call =
  { X86.Block_call.label = block_call.Block_call.label
  ; args = List.map ~f:(lower_value_reg cx) block_call.args
  }

and lower_block_args cx (block_args : Block_args.t) =
  let block_args = List.map ~f:(fun v -> vreg v) block_args in
  Cx.addv cx @@ Block_args block_args

and lower_control_instr cx instr =
  match instr with
  | Control_instr.CondJump (v, bc1, bc2) ->
    let op1 = lower_value_op cx v in
    let bc1 = lower_block_call cx bc1 in
    let bc2 = lower_block_call cx bc2 in
    Cx.add cx @@ Test { src1 = op1; src2 = op1 };
    Cx.addj cx @@ X86.Jump.CondJump { cond = X86.Cond.NE; j1 = bc1; j2 = bc2 }
  | Control_instr.Jump j ->
    let j = lower_block_call cx j in
    Cx.addj cx @@ X86.Jump.Jump j
  | Ret v ->
    let v = Option.map ~f:(lower_value_op cx) v in
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
  let params, stack_params = categorize_args fn.params in
  let params =
    List.map params ~f:(fun (value, mach_reg) ->
      vreg value, X86.MReg.create (value_size value) mach_reg)
  in
  let stack_params = List.map stack_params ~f:vreg in
  { X86.Function.graph
  ; caller_saved = X86.Mach_reg.caller_saved (* ; params = *)
  ; params
  ; stack_params
  ; unique_name = cx.unique_name
  ; stack_instrs = cx.stack_instrs
  }
;;

let lower (prog : Tir.Program.t) =
  let functions = List.map ~f:lower_function prog.functions in
  { X86.Program.functions }
;;
