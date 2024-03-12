open O
open Ast

let instr_to_block_transfer ?(sexp_of_reg = sexp_of_opaque) trans =
  Cfg.Dataflow.instr_to_block_transfer
    ~sexp_of_block:(Block.sexp_of_t sexp_of_reg)
    ~iter_instrs_forward:Block.iter_instrs_forward
    ~iter_instrs_backward:Block.iter_instrs_backward
    trans
;;

let run_block_transfer transfer (graph : _ Graph.t) =
  Cfg.Dataflow.run_block_transfer transfer
  @@ Cfg.Dataflow.Graph.of_cfg ~jumps:Block.iter_jumps graph
;;

module Liveness = struct
  let instr_transfer =
    Cfg.Dataflow.Liveness.make_transfer
      ~sexp_of_instr:[%sexp_of: VReg.t Instr.t]
      ~value:(module VReg)
      ~uses:Instr.iter_uses
      ~defs:Instr.iter_defs
  ;;

  let block_transfer = instr_to_block_transfer instr_transfer
  let run fn = run_block_transfer block_transfer fn.Function.graph
end
