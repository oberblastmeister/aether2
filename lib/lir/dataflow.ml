open! O
include Types

let instr_to_block_transfer (type a) (module Value : Value with type t = a) =
  Cfg.Dataflow.instr_to_block_transfer
    ~sexp_of_block:[%sexp_of: Value.t Block.t]
    ~instrs_forward_fold:Block.instrs_forward_fold
    ~instrs_backward_fold:Block.instrs_backward_fold
;;

let run_block_transfer transfer (graph : _ Graph.t) =
  Cfg.Dataflow.run_block_transfer transfer
  @@ Cfg.Dataflow.Graph.of_cfg ~jumps:Block.jumps_fold graph
;;
