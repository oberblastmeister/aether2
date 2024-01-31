open! O
open Utils.Instr_types
module T = Types_basic
module Name = Name
module Label = Label
module Control = Control

module Reg = struct
  include T.Reg
end

module Cond = struct
  include T.Cond
end

module MachReg = struct
  include T.MachReg
end

module VReg = struct
  include T.VReg
end

module Size = struct
  include T.Size
end

module Address = struct
  include T.Address

  let regs_fold a k =
    k a.base;
    k a.index
  ;;
end

module Operand = struct
  include T.Operand

  let reg_val_fold o k = reg_val o |> Option.map ~f:k |> ignore
  let mem_val_fold o k = mem_val o |> Option.map ~f:k |> ignore
  let mem_regs_fold o k = (mem_val_fold @> Address.regs_fold) o k

  let any_regs_fold o k =
    match o with
    | Reg r -> k r
    | Mem m -> Address.regs_fold m k
    | Imm _ -> ()
  ;;
end

module Mov = struct
  include T.Mov

  let defs_fold mov k = Operand.reg_val_fold mov.dst k

  let uses_fold mov k =
    Operand.mem_regs_fold mov.dst k;
    Operand.any_regs_fold mov.src k
  ;;
end

module Instr = struct
  include T.Instr

  let defs_fold i k =
    let module O = Operand in
    match i with
    | Def { dst; _ } -> O.reg_val_fold dst k
    | Mov mov -> O.reg_val_fold mov.dst k
    | Par_mov movs -> (FC.List.fold @> F.Fold.of_fn Mov.dst @> O.reg_val_fold) movs k
    | Add { dst; _ } -> O.reg_val_fold dst k
    | Lea { dst; _ } -> k dst
    | Cmp _ | _ -> ()
  ;;

  let uses_fold i k =
    let module O = Operand in
    match i with
    | Add { dst; src1; src2; _ } ->
      O.mem_regs_fold dst k;
      O.any_regs_fold src1 k;
      O.any_regs_fold src2 k
    | Mov mov -> Mov.uses_fold mov k
    | Par_mov movs -> (FC.List.fold @> Mov.uses_fold) movs k
    | Cmp { src1; src2; _ } ->
      O.any_regs_fold src1 k;
      O.any_regs_fold src2 k
    | Lea { src; _ } -> Address.regs_fold src k
    | Def { dst } -> O.mem_regs_fold dst k
    | _ -> ()
  ;;
end

module Block_call = struct
  include T.Block_call
end

module Procedure = struct
  include T.Procedure
end

module Program = struct
  include T.Program
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
    | Jump j -> k j.label
    | CondJump { j1; j2; _ } ->
      k j1.label;
      k j2.label
    | _ ->
      raise_s [%message "instruction was not control instr" ~instr:(last_instr : Instr.t)]
  ;;

  let instrs_forward_fold b = Vec.to_iter b.instrs
  let instrs_backward_fold b = Vec.to_iter_rev b.instrs
end

module Graph = struct
  include T.Graph
end

module Dataflow = struct
  let instr_to_block_transfer trans =
    Cfg.Dataflow.instr_to_block_transfer
      ~sexp_of_block:[%sexp_of: Block.t]
      ~instrs_forward_fold:Block.instrs_forward_fold
      ~instrs_backward_fold:Block.instrs_backward_fold
      trans
  ;;

  let run_block_transfer transfer (graph : Graph.t) =
    Cfg.Dataflow.run_block_transfer transfer
    @@ Cfg.Dataflow.Graph.of_cfg ~jumps:Block.jumps_fold graph
  ;;

  module Liveness = struct
    let instr_transfer =
      Cfg.Dataflow.Liveness.make_transfer
        ~sexp_of_instr:[%sexp_of: Instr.t]
        ~value:(module Reg)
        ~uses:Instr.uses_fold
        ~defs:Instr.defs_fold
    ;;

    let block_transfer = instr_to_block_transfer instr_transfer
    let run fn = run_block_transfer block_transfer fn.Procedure.graph
  end
end
