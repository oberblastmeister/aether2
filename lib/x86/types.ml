open! O
open Utils.Instr_types
module T = Types_basic
module Name = Name
module Label = Label
module Control = Control

module Cond = struct
  include T.Cond
end

module Mach_reg = struct
  include T.Mach_reg
end

module MReg = struct
  include T.MReg
end

module VReg = struct
  include T.VReg
end

module Size = struct
  include T.Size

  let to_byte_size = function
    | Q -> 8
    | L -> 4
    | W -> 2
    | B -> 1
  ;;
end

module Address = struct
  include T.Address

  let regs_fold a k =
    k a.base;
    k a.index
  ;;
end

module Imm = struct
  include T.Imm
end

module Operand = struct
  include T.Operand

  let reg_val_fold o k = reg_val o |> Option.iter ~f:k
  let mem_val_fold o k = mem_val o |> Option.iter ~f:k
  let mem_regs_fold o k = (mem_val_fold @> Address.regs_fold) o k

  let any_regs_fold o k =
    match o with
    | Reg r -> k r
    | Mem m -> Address.regs_fold m k
    | Imm _ -> ()
  ;;

  (* let mach_reg s r = Reg.mach_reg s r |> reg *)
end

module Block_call = struct
  include T.Block_call

  let uses_fold (type r) (instr : r t) (k : r -> unit) =
    match instr.args with
    | Block_call args -> List.iter args ~f:k
    | No_block_call -> ()
  ;;

  let regs_fold block_call k = uses_fold block_call k
end

module Maybe_block_call = struct
  include T.Maybe_block_call
end

module Jump = struct
  include T.Jump

  let regs_fold i k =
    match i with
    | Jump j -> Block_call.regs_fold j k
    | CondJump { j1; j2; _ } ->
      Block_call.regs_fold j1 k;
      Block_call.regs_fold j2 k
  ;;

  let uses_fold i k =
    match i with
    | Jump j -> Block_call.uses_fold j k
    | CondJump { j1; j2; _ } ->
      Block_call.uses_fold j1 k;
      Block_call.uses_fold j2 k
  ;;
end

module VInstr = struct
  include T.VInstr

  let regs_fold i k =
    let module O = Operand in
    let module A = Address in
    match i with
    | Par_mov movs -> (FC.List.fold @> FC.Tuple2.fold_both) movs k
    | Def { dst; _ } -> k dst
    | Block_args regs -> FC.List.fold regs k
    | ReserveStackEnd _ -> ()
  ;;

  let defs_fold i k =
    let module O = Operand in
    match i with
    | Def { dst; _ } -> k dst
    | Par_mov movs -> (FC.List.fold @> F.Fold.of_fn fst) movs k
    | Block_args args -> FC.List.fold args k
    | ReserveStackEnd _ -> ()
  ;;

  let uses_fold i k =
    let module O = Operand in
    match i with
    | Par_mov movs -> (FC.List.fold @> F.Fold.of_fn snd) movs k
    | Block_args _ -> ()
    | Def _ | ReserveStackEnd _ -> ()
  ;;
end

module MInstr = struct
  include T.MInstr

  let regs_fold (type r) (i : r t) (k : r -> unit) =
    let module O = Operand in
    let module A = Address in
    match i with
    | Lea { dst; src; _ } ->
      k dst;
      A.regs_fold src k
    | Add { dst; src1; src2; _ } ->
      O.any_regs_fold dst k;
      O.any_regs_fold src1 k;
      O.any_regs_fold src2 k
    | MovImm64 { dst; _ } -> O.any_regs_fold dst k
    | Mov { dst; src; _ } ->
      O.any_regs_fold dst k;
      O.any_regs_fold src k
    | Cmp { src1; src2; _ } | Test { src1; src2; _ } ->
      O.any_regs_fold src1 k;
      O.any_regs_fold src2 k
    | Set { dst; _ } -> O.any_regs_fold dst k
    | Ret -> ()
    | Push { src; _ } -> k src
    | Pop { dst; _ } -> k dst
  ;;

  let defs_fold i k =
    let module O = Operand in
    let module A = Address in
    match i with
    | Mov mov -> O.reg_val_fold mov.dst k
    | MovImm64 { dst; _ } -> O.reg_val_fold dst k
    | Add { dst; _ } -> O.reg_val_fold dst k
    | Lea { dst; _ } -> k dst
    | Set { dst; _ } -> O.reg_val_fold dst k
    | Pop { dst; _ } -> k dst
    | Push _ | Cmp _ | Test _ | Ret -> ()
  ;;

  let uses_fold i k =
    let module O = Operand in
    let module A = Address in
    match i with
    | Add { dst; src1; src2; _ } ->
      O.mem_regs_fold dst k;
      O.any_regs_fold src1 k;
      O.any_regs_fold src2 k
    | Mov { dst; src; _ } ->
      O.mem_regs_fold dst k;
      O.any_regs_fold src k
    | Lea { src; _ } -> Address.regs_fold src k
    | MovImm64 { dst; _ } -> O.mem_regs_fold dst k
    | Cmp { src1; src2; _ } | Test { src1; src2; _ } ->
      O.any_regs_fold src1 k;
      O.any_regs_fold src2 k
    | Set { dst; _ } -> O.mem_regs_fold dst k
    | Push { src; _ } -> k src
    | Pop _ | Ret -> ()
  ;;
end

module Instr_variant = struct
  include T.Instr_variant
end

module Instr = struct
  include T.Instr

  let to_variant = function
    | Virt v -> Instr_variant.Virt v
    | Real r -> Instr_variant.Real r
    | Jump j -> Instr_variant.Jump j
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
end

module Block = struct
  include T.Block

  let jumps_fold block k =
    let last_instr =
      Vec.last block.instrs
      |> Option.value_or_thunk ~default:(fun () ->
        raise_s [%message "block must have a last instr"])
    in
    match last_instr with
    | Instr.Jump (Jump j) -> k j.label
    | Instr.Jump (CondJump { j1; j2; _ }) ->
      k j1.label;
      k j2.label
    | _ ->
      raise_s
        [%message "instruction was not control instr" ~instr:(last_instr : _ Instr.t)]
  ;;

  let instrs_forward_fold b = Vec.to_iter b.instrs
  let instrs_backward_fold b = Vec.to_iter_rev b.instrs
end

module Graph = struct
  include T.Graph

  include Cfg.Graph.Make_gen (struct
      type 'r t = 'r Block.t

      let jumps_fold = Block.jumps_fold
    end)
end

module Function = struct
  include T.Function

  let instrs_forward_fold fn = (FC.Map.fold @> Block.instrs_forward_fold) fn.graph.blocks
end

module Program = struct
  include T.Program
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
