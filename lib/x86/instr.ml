open! O
open Utils.Instr_types
module T = Instr_types
module Name = Name
module Label = Label
module Control = Control

module Reg = struct
  include T.Reg
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
    | Cmp { dst; src; _ } ->
      O.any_regs_fold dst k;
      O.any_regs_fold src k
    | Lea { src; _ } -> Address.regs_fold src k
    | Def { dst } -> O.mem_regs_fold dst k
    | _ -> ()
  ;;
end

module Block = struct
  include T.Block

  let instrs_forward_fold b = Vec.to_iter b.instrs
  let instrs_backward_fold _ = todo ()
end

module Graph = struct
  include T.Graph
end

module Dataflow = struct
  let instr_to_block_transfer trans =
    Dataflow.instr_to_block_transfer
      ~sexp_of_block:[%sexp_of: Block.t]
      { instrs_forward_fold = Block.instrs_forward_fold
      ; instrs_backward_fold = Block.instrs_backward_fold
      }
      trans
  ;;

  let run_block_transfer transfer (graph : Graph.t) =
    Dataflow.run_block_transfer
      transfer
      { entry = graph.entry
      ; v = Graph.to_double_graph graph
      ; exit = graph.exit
      ; get_block = Map.find_exn graph.blocks
      }
  ;;

  module Liveness = struct
    let dict =
      { Dataflow.value = (module Reg); uses = Instr.uses_fold; defs = Instr.defs_fold }
    ;;
  end
end
