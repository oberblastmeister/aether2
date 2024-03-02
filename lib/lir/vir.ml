open! O

module Lir = struct
  include Instantiate
  include Types
end

include Lir.Instantiate (struct
    include Lir.Value
  end)

module Liveness = struct
  let instr_transfer =
    Cfg.Dataflow.Liveness.make_transfer
      ~sexp_of_instr:[%sexp_of: Some_instr.t]
      ~value:(module Lir.Value)
      ~uses:Lir.Some_instr.uses_fold
      ~defs:Lir.Some_instr.defs_fold
  ;;

  let block_transfer = Dataflow.instr_to_block_transfer (module Lir.Value) instr_transfer
  let run = Dataflow.run_block_transfer block_transfer
end
