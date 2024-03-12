open! O
include Ast

let instr_to_block_transfer (type a) (module Value : Value with type t = a) =
  Cfg.Dataflow.instr_to_block_transfer
    ~sexp_of_block:[%sexp_of: Value.t Block.t]
    ~iter_instrs_forward:Block.iter_instrs_forward
    ~iter_instrs_backward:Block.iter_instrs_backward
;;

let run_block_transfer transfer (graph : _ Graph.t) =
  Cfg.Dataflow.run_block_transfer transfer
  @@ Cfg.Dataflow.Graph.of_cfg ~jumps:Block.iter_jumps graph
;;
