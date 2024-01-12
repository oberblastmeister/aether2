open O

include Lir_instantiate.Instantiate (struct
    include Lir_instr.Value
  end)

module Liveness = struct
  let dict =
    { Dataflow.value = (module Lir_instr.Value)
    ; uses = Lir_instr.Some_instr.uses_fold
    ; defs = Lir_instr.Some_instr.defs_fold
    }
  ;;

  let instr_transfer = Dataflow.Liveness.make_transfer (module Some_instr) dict

  let block_transfer =
    Lir_instr.Dataflow.instr_to_block_transfer (module Lir_instr.Value) instr_transfer
  ;;

  let run = Lir_instr.Dataflow.run_block_transfer block_transfer
end

module DataflowDominators = struct
  let block_transfer = Dataflow.Dominators.make_transfer (module Block)
  let run = Lir_instr.Dataflow.run_block_transfer block_transfer
end
