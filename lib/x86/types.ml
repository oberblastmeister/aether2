open! O
include Types_basic
module Name = Utils.Instr_types.Name
module Label = Utils.Instr_types.Label
module Control = Utils.Instr_types.Control
module Mach_reg_set = Data.Enum_set.Make (Mach_reg)

module Mach_reg = struct
  include Mach_reg

  let caller_saved_without_r11 = [ RAX; RDI; RSI; RDX; RCX; R8; R9; R10 ]
  let caller_saved = caller_saved_without_r11 @ [ R11 ]
  let callee_saved_without_stack = [ RBX; R12; R13; R14; R15 ]
  let callee_saved = [ RSP; RBP ] @ callee_saved_without_stack
  let args = [ RDI; RSI; RDX; RCX; R8; R9 ]
  let ret = RAX
end

module AReg = struct
  include AReg

  let reg_val = function
    | InReg { reg; _ } -> Some reg
    | _ -> None
  ;;

  let size (Spilled { s; _ } | InReg { s; _ }) = s
  let create ?name s reg = InReg { s; name; reg }

  (* let in_reg s name reg = { s; name; reg = Some reg }
  let spilled s name = { s; name; reg = None }
  let is_spilled t = Option.is_none t.reg
  let is_in_reg t = Option.is_some t.reg

  let to_spilled t =
    match t.reg with
    | None -> Some (t.s, t.name)
    | Some _ -> None
  ;; *)
end

module VReg = struct
  include VReg

  (* let to_name r = r.name *)
  let create s name = { s; name }
  let precolored s name = { s; name }
end

module Size = struct
  include Size

  let to_byte_size = function
    | Q -> 8
    | L -> 4
    | W -> 2
    | B -> 1
  ;;
end

module Stack_off = struct
  include Stack_off
end

module Address = struct
  include Address

  module Base = struct
    include Base

    let regs_fold a k = reg_val a |> Option.iter ~f:k
  end

  module Scale = struct
    include Scale
  end

  module Index = struct
    include Index

    let regs_fold a k =
      some_val a |> Option.iter ~f:(fun (`index index, `scale _) -> k index)
    ;;
  end

  let stack_local name = Imm { offset = Stack (Local name); scale = One }
  let base base = Complex { base; index = None; offset = Int 0l }

  let index_scale index scale =
    Complex { base = None; index = Some { index; scale }; offset = Int 0l }
  ;;

  let base_offset base offset = Complex { base; index = None; offset }
  let rip_relative offset = base_offset Base.Rip offset

  let regs_fold a k =
    match a with
    | Imm _ -> ()
    | Complex { base; index; _ } ->
      Base.regs_fold base k;
      Index.regs_fold index k
  ;;
end

module Mem = struct
  include Mem

  let regs_fold a = Address.regs_fold a.addr
end

module Operand = struct
  include Operand

  let imm i = Imm (Imm.Int i)
  let stack_off_end i = Imm (Imm.Stack (Stack_off.End i))
  let stack_local s name = Operand.mem s (Address.stack_local name)
  let vreg s name = Reg (VReg.create s name)
  let precolored s name = Reg (VReg.precolored s name)
  let reg_val_fold o k = reg_val o |> Option.iter ~f:k
  let mem_val_fold o k = mem_val o |> Option.iter ~f:k
  let mem_regs_fold o k = (mem_val_fold @> Mem.regs_fold) o k

  let any_regs_fold o k =
    match o with
    | Reg r -> k r
    | Mem m -> Mem.regs_fold m k
    | Imm _ -> ()
  ;;

  let of_areg = function
    | AReg.InReg { name; reg; s } -> Reg (MReg.create ?name s reg)
    | AReg.Spilled { s; name } -> stack_local s name
  ;;
end

module Block_call = struct
  include Block_call

  let uses_fold (type r) (instr : r t) (k : r -> unit) = instr.args |> List.iter ~f:k
  let map_regs i ~f = map f i
end

module Jump = struct
  include Jump

  let map_regs i ~f = map f i

  let uses_fold i k =
    match i with
    | Jump j -> Block_call.uses_fold j k
    | CondJump { j1; j2; _ } ->
      Block_call.uses_fold j1 k;
      Block_call.uses_fold j2 k
    | Ret r -> Option.iter r ~f:(fun o -> Operand.any_regs_fold o k)
  ;;

  let block_calls_fold j k =
    match j with
    | Jump j -> k j
    | CondJump { j1; j2; _ } ->
      k j1;
      k j2
    | Ret _ -> ()
  ;;

  let map_block_calls j ~f =
    match j with
    | Jump j -> Jump (f j)
    | CondJump { j1; j2; cond } -> CondJump { j1 = f j1; j2 = f j2; cond }
    | Ret r -> Ret r
  ;;

  let map_regs i = (map_block_calls & Block_call.map_regs) i
end

module VInstr = struct
  include VInstr

  let map_regs i ~f = map f i

  let regs_fold i k =
    let module O = Operand in
    let module A = Address in
    match i with
    | Par_mov movs -> (FC.List.fold @> FC.Tuple2.fold_both) movs k
    | Block_args regs -> FC.List.fold regs k
    | ReserveStackEnd _ | ReserveStackLocal _ -> ()
  ;;

  let defs_fold i k =
    let module O = Operand in
    match i with
    | Par_mov movs -> (FC.List.fold @> F.Fold.of_fn fst) movs k
    | Block_args args -> FC.List.fold args k
    | ReserveStackEnd _ | ReserveStackLocal _ -> ()
  ;;

  let uses_fold i k =
    let module O = Operand in
    match i with
    | Par_mov movs -> (FC.List.fold @> F.Fold.of_fn snd) movs k
    | Block_args _ -> ()
    | ReserveStackEnd _ | ReserveStackLocal _ -> ()
  ;;
end

module Instr = struct
  include Instr

  (* add a on_mach_reg parameter *)
  (* then add a specialized fold_mach_regs *)
  let operands_fold_with i ~on_def ~on_use =
    let module O = Operand in
    let module A = Address in
    match i with
    | NoOp -> ()
    | Lea { dst; src; _ } ->
      on_def @@ O.Reg dst;
      on_def @@ O.Reg dst;
      Address.regs_fold src (fun reg -> on_use (O.Reg reg))
    | Add { dst; src1; src2; _ } ->
      on_def dst;
      on_use src1;
      on_use src2
    | MovAbs { dst; _ } -> on_def dst
    | Mov { dst; src; _ } ->
      on_def dst;
      on_use src
    | Cmp { src1; src2; _ } | Test { src1; src2; _ } ->
      on_use src1;
      on_use src2
    | Set { dst; _ } -> on_def dst
    | Push { src; _ } -> on_use @@ O.Reg src
    | Pop { dst; _ } -> on_def @@ O.Reg dst
    | Call { reg_args; dst; _ } ->
      List.iter reg_args ~f:(fun (_, arg) -> on_use (Reg arg));
      on_def (Reg dst)
    | Jump jump -> Jump.uses_fold jump (fun reg -> on_use (Reg reg))
    | Virt vinstr ->
      VInstr.uses_fold vinstr (fun reg -> on_use (Reg reg));
      VInstr.defs_fold vinstr (fun reg -> on_def (Reg reg))
  ;;

  (* type 'r mapper = { f : 'op. ('r, 'op) GOperand.t -> ('r, 'op) GOperand.t } *)

  (* let map_operands i ~f:{ f = gmap } =
    let map_op (o : 'a Operand.t) =
      match o with
      | Operand.Reg r -> GOperand.to_operand (gmap (Reg r))
      | Mem m -> GOperand.to_operand (gmap (Mem m))
      | Imm i -> GOperand.to_operand (gmap (Imm i))
    in
    let map_address a = gmap (Mem a) |> GOperand.mem_val in
    let map_imm i = gmap (Imm i) |> GOperand.imm_val in
    let map_reg r = gmap (Reg r) |> GOperand.reg_val in
    match i with
    | NoOp -> NoOp
    | Mov { s; dst; src } -> Mov { s; dst = map_op dst; src = map_op src }
    | Lea { dst; src; s } -> Lea { dst = map_reg dst; src = map_address src; s }
    | Add { dst; src1; src2; s } ->
      Add { dst = map_op dst; src1 = map_op src1; src2 = map_op src2; s }
    | Push { src; s } -> Push { src = map_reg src; s }
    | Pop { dst; s } -> Pop { dst = map_reg dst; s }
    | MovAbs { dst; imm } -> MovAbs { dst = map_op dst; imm }
    | Cmp { s; src1; src2 } -> Cmp { s; src1 = map_op src1; src2 = map_op src2 }
    | Test { s; src1; src2; _ } -> Test { s; src1 = map_op src1; src2 = map_op src2 }
    | Set { s; dst; cond } -> Set { s; dst = map_op dst; cond }
    | Call p -> Call p
    | Jump j -> Jump j
    | Virt v -> Virt v *)

  let operands_fold i k = operands_fold_with i ~on_def:k ~on_use:k
  let regs_fold i k = operands_fold i (fun o -> Operand.any_regs_fold o k)
  let map_regs i ~f = map f i

  let defs_fold i k =
    operands_fold_with i ~on_def:(fun o -> Operand.reg_val_fold o k) ~on_use:(Fn.const ())
  ;;

  let uses_fold i k =
    operands_fold_with
      i
      ~on_def:(fun o -> Operand.mem_regs_fold o k)
      ~on_use:(fun o -> Operand.any_regs_fold o k)
  ;;

  let mov_to_reg_from_stack s reg (stack_name : Name.t) =
    Mov
      { dst = Reg (MReg.create ~name:stack_name.name s reg)
      ; src = Operand.mem s (Address.stack_local stack_name)
      }
  ;;

  let mov_to_stack_from_reg s (stack_name : Name.t) reg =
    Mov
      { dst = Operand.mem s (Address.stack_local stack_name)
      ; src = Reg (MReg.create ~name:stack_name.name s reg)
      }
  ;;

  let mach_reg_defs i k =
    match i with
    | NoOp
    | Mov _
    | Lea _
    | Add _
    | Push _
    | Pop _
    | MovAbs _
    | Cmp _
    | Test _
    | Set _
    | Jump _
    | Virt _ -> ()
    | Call { defines; _ } -> List.iter defines ~f:k
  ;;
end

(* module Instr_variant = struct
   include Instr_variant
   end

   module Instr = struct
   include Instr

   let to_variant (type r) (i : r t) =
   match i with
   | Virt v -> Instr_variant.Virt v
   | Real r -> Instr_variant.Real r
   | Jump j -> Instr_variant.Jump j
   ;;

   let map_regs i ~f =
   match i with
   | Virt i -> Virt (VInstr.map_regs i ~f)
   | Real i -> Real (MInstr.map_regs i ~f)
   | Jump i -> Jump (Jump.map_regs i ~f)
   ;;

   let regs_fold (type r) (i : r t) (k : r -> unit) =
   let module O = Operand in
   let module A = Address in
   match i with
   | Virt i -> VInstr.regs_fold i k
   | Real i -> MInstr.regs_fold i k
   | Jump i -> Jump.regs_fold i k
   ;;

   let defs_fold i k =
   let module O = Operand in
   let open Instr_variant in
   match to_variant i with
   | Virt i -> VInstr.defs_fold i k
   | Real i -> MInstr.defs_fold i k
   | Jump _ -> ()
   ;;

   let uses_fold (type r) (i : r t) (k : r -> unit) =
   let module O = Operand in
   let open Instr_variant in
   match i with
   | Virt i -> VInstr.uses_fold i k
   | Real i -> MInstr.uses_fold i k
   | Jump i -> Jump.uses_fold i k
   ;;

   end *)

module Block = struct
  include Block

  let first_instr b = Vec.first b.instrs

  let jump_exn block =
    Vec.last block.instrs
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "block must have a last instr"])
    |> fun instr ->
    Instr.jump_val instr
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "last instr must be a jump" (instr : _ Instr.t)])
  ;;

  let block_args_exn block =
    Vec.first block.instrs
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "block must have a first instr"])
    |> fun instr ->
    instr
    |> fun instr ->
    (let open Option.Let_syntax in
     let%bind virt = Instr.virt_val instr in
     let%bind block_args = VInstr.block_args_val virt in
     return block_args)
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "first instruction must be block arguments" (instr : _ Instr.t)])
  ;;

  let cons instr b = { instrs = Vec.cons instr b.instrs }

  let jumps_fold block k =
    match jump_exn block with
    | Jump j -> k j.label
    | CondJump { j1; j2; _ } ->
      k j1.label;
      k j2.label
    | Ret _ -> ()
  ;;

  let instrs_forward_fold b = Vec.to_iter b.instrs
  let instrs_backward_fold b = Vec.to_iter_rev b.instrs
end

module Graph = struct
  include Graph

  include Cfg.Graph.Make_gen (struct
      type 'r t = 'r Block.t

      let jumps_fold = Block.jumps_fold
    end)

  let map_blocks = Cfg.Graph.map
end

module Function = struct
  include Function

  let instrs_forward_fold fn = (Cfg.Graph.to_iter @> Block.instrs_forward_fold) fn.graph
  let map_blocks fn ~f = { fn with graph = Graph.map_blocks fn.graph ~f }
end

module Program = struct
  include Program

  let map_functions program ~f = { functions = List.map program.functions ~f }
  let iter_functions program ~f = List.iter program.functions ~f
end

module Dataflow = struct
  let instr_to_block_transfer ?(sexp_of_reg = sexp_of_opaque) trans =
    Cfg.Dataflow.instr_to_block_transfer
      ~sexp_of_block:(Block.sexp_of_t sexp_of_reg)
      ~instrs_forward_fold:Block.instrs_forward_fold
      ~instrs_backward_fold:Block.instrs_backward_fold
      trans
  ;;

  let run_block_transfer transfer (graph : _ Graph.t) =
    Cfg.Dataflow.run_block_transfer transfer
    @@ Cfg.Dataflow.Graph.of_cfg ~jumps:Block.jumps_fold graph
  ;;

  module Liveness = struct
    let instr_transfer =
      Cfg.Dataflow.Liveness.make_transfer
        ~sexp_of_instr:[%sexp_of: VReg.t Instr.t]
        ~value:(module VReg)
        ~uses:Instr.uses_fold
        ~defs:Instr.defs_fold
    ;;

    let block_transfer = instr_to_block_transfer instr_transfer
    let run fn = run_block_transfer block_transfer fn.Function.graph
  end
end
