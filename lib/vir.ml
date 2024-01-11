module Vir = struct
  include Lir_instantiate.InstantiateWithDataflow (struct
      include Lir_instr.Value
    end)

  module Liveness = struct
    module InstrTransfer = Cfg.Dataflow.Liveness.Make (Dataflow.Instr)

    module BlockTransfer =
      Cfg.Dataflow.InstrToBlockTransfer (Dataflow.Block) (InstrTransfer)

    include Dataflow.Framework.MakeAnalysis (BlockTransfer)
  end
end

include Vir
