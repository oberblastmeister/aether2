open! O
open Ast

val instr_to_block_transfer
  :  (module Value with type t = 'v)
  -> ('v Some_instr.t, 'd) Cfg.Dataflow.Instr_transfer.t
  -> ('v Block.t, 'd) Cfg.Dataflow.Block_transfer.t

val run_block_transfer
  :  ('v Block.t, 'd) Cfg.Dataflow.Block_transfer.t
  -> 'v Graph.t
  -> 'd Cfg.Dataflow.Fact_base.t * 'd Cfg.Dataflow.Fact_base.t
