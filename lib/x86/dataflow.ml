open O
open Types

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
