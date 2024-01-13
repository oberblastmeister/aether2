open O

module Lir = struct
  include Lir_instantiate
  include Lir_instr
end

include Lir.Instantiate (struct
    include Lir.Value
  end)

module Liveness = struct
  let dict =
    { Dataflow.value = (module Lir.Value)
    ; uses = Lir.Some_instr.uses_fold
    ; defs = Lir.Some_instr.defs_fold
    }
  ;;

  let instr_transfer = Dataflow.Liveness.make_transfer (module Some_instr) dict

  let block_transfer =
    Lir.Dataflow.instr_to_block_transfer (module Lir.Value) instr_transfer
  ;;

  let run = Lir.Dataflow.run_block_transfer block_transfer
end

module DataflowDominators = struct
  let block_transfer = Dataflow.Dominators.make_transfer (module Block)
  let run = Lir.Dataflow.run_block_transfer block_transfer
end
