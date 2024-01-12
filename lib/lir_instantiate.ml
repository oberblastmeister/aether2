open O
module Lir = Lir_instr

module Instantiate (V : sig
    type t [@@deriving sexp_of]
  end) =
struct
  module Instr = struct
    type t = V.t Lir.Instr.t [@@deriving sexp_of]
  end

  module Block_call = struct
    type t = V.t Lir.Block_call.t [@@deriving sexp_of]
  end

  module Control_instr = struct
    type t = V.t Lir.Control_instr.t [@@deriving sexp_of]
  end

  module Some_instr = struct
    type t = V.t Lir.Some_instr.t [@@deriving sexp_of]
  end

  module Generic_instr = struct
    type 'c t = ('c, V.t) Lir.Generic_instr.t
  end

  module Block = struct
    type t = V.t Lir.Block.t [@@deriving sexp_of]
  end

  (* module DataGraph = Lir.Graph.MakeDataGraph (V) *)
  module DataGraph = Cfg_graph.MakeDataGraph (struct
      type t = V.t Lir.Block.t [@@deriving sexp_of]

      let jumps_fold b = Lir.Block.jumps_fold b
      let jumps b = Lir.Block.jumps b
    end)

  module Dfs = Data_graph.Dfs (DataGraph)

  module Graph = struct
    type t = V.t Lir.Graph.t [@@deriving sexp_of]
  end

  module Mut_function = struct
    type t = V.t Lir.Mut_function.t
  end

  module Function = struct
    type t = V.t Lir.Function.t [@@deriving sexp_of]
  end

  module Program = struct
    type t = V.t Lir.Program.t [@@deriving sexp_of]
  end
end

module InstantiateWithDataflow (V : sig
    type t [@@deriving equal, compare, hash, sexp]

    include Comparable.S with type t := t
  end) =
struct
  module M = Instantiate (V)

  module Dataflow = struct
    module Instr = struct
      module Value = V

      type t = V.t Lir.Some_instr.t [@@deriving sexp_of]

      let uses = Lir.Some_instr.uses
      let defs = Lir.Some_instr.defs
    end

    module Block = struct
      include Lir.Block

      type t = V.t Lir.Block.t [@@deriving sexp_of]

      module Node = Lir.Label
      module Instr = Instr
    end

    module Framework = Cfg.Dataflow.Make (struct
        include M.DataGraph
        module Block = Block

        let exit (graph : M.Graph.t) = graph.exit
        let exit (graph : M.Graph.t) = graph.exit
        let get_block (graph : M.Graph.t) label = Map.find_exn graph.blocks label
      end)
  end

  module DataflowDominators = struct
    module BlockTransfer = Cfg.Dataflow.Dominators.Make (Dataflow.Block)
    include Cfg.Dataflow.Dominators.MakeHelpers (Dataflow.Block)
    include Dataflow.Framework.MakeAnalysis (BlockTransfer)
  end

  module Dominators = Dominators.MakeDominators (Lir.Graph.DataGraph)
  include M
end
