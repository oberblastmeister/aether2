open O

module type Instr = sig
  type t [@@deriving sexp_of]
end

module type Value = sig
  type t [@@deriving equal, compare, sexp, hash]

  include Comparable.S with type t := t
end

module type InstrWithValue = sig
  include Instr
  module Value : Value

  val uses : t -> Value.t list
  val defs : t -> Value.t list
end

module type Block = sig
  type t [@@deriving sexp_of]

  module Node : sig
    type t [@@deriving equal, compare, hash, sexp]

    include Comparable.S with type t := t
  end

  module Instr : Instr

  val jumps : t -> Node.t list
  val fold_instrs_forward : t -> init:'a -> f:('a -> Instr.t -> 'a) -> 'a
  val fold_instrs_backward : t -> init:'a -> f:('a -> Instr.t -> 'a) -> 'a
end

module type Graph = sig
  include Data_graph.Graph
  module Block : Block with module Node = Node

  val entry : t -> Node.t
  val exit : t -> Node.t
  val get_block : t -> Node.t -> Block.t
end

module type Domain = sig
  type t [@@deriving sexp_of]
end

module Direction = struct
  type t =
    | Forward
    | Backward
end

module type InstrTransfer = sig
  module Instr : Instr
  module Domain : Domain

  val transfer : Instr.t -> Domain.t -> Domain.t
  val changed : current_fact:Domain.t -> new_fact:Domain.t -> bool
  val empty : Domain.t
  val combine : Domain.t list -> Domain.t
  val direction : Direction.t
end

module type BlockTransfer = sig
  module Block : Block
  module Domain : Domain

  val transfer
    :  Block.Node.t
    -> Block.t
    -> other_facts:Domain.t list
    -> current_fact:Domain.t
    -> Domain.t Option.t

  val empty : Domain.t
  val direction : Direction.t
end

module type ConvertedInstrTransfer = sig
  include BlockTransfer
  module InstrTransfer : InstrTransfer
end

module type S = sig
  module Graph : Graph

  module MakeAnalysis : functor
      (Transfer : BlockTransfer with module Block = Graph.Block)
      -> sig
    val run : Graph.t -> Transfer.Domain.t Graph.Node.Map.t
  end
end

module DominatorHelpers = struct
  module type S = sig
    module Block : Block

    val compute_idoms_from_facts
      :  Block.Node.t
      -> Block.Node.Set.t Block.Node.Map.t
      -> Block.Node.t Block.Node.Map.t

    val compute_idom_tree_from_facts
      :  Block.Node.t
      -> Block.Node.Set.t Block.Node.Map.t
      -> Block.Node.Set.t Block.Node.Map.t
  end
end

module type Intf = sig
  module type Instr = Instr
  module type InstrWithValue = InstrWithValue
  module type Block = Block
  module type Graph = Graph
  module type Domain = Domain
  module type InstrTransfer = InstrTransfer
  module type BlockTransfer = BlockTransfer
  module type ConvertedInstrTransfer = ConvertedInstrTransfer

  module DominatorHelpers : module type of DominatorHelpers

  module Direction : sig
    type t = Direction.t
  end

  module InstrToBlockTransfer : functor
      (Block : Block)
      (Transfer : InstrTransfer with module Instr = Block.Instr)
      ->
    ConvertedInstrTransfer
    with module Block = Block
     and module Domain = Transfer.Domain
     and module InstrTransfer = Transfer

  module Make : functor (Graph : Graph) -> S with module Graph := Graph

  module Liveness : sig
    module Make : functor (Instr : InstrWithValue) ->
      InstrTransfer with module Instr = Instr and module Domain = Instr.Value.Set
  end

  module Dominators : sig
    module Make : functor (Block : Block) ->
      BlockTransfer with module Block = Block and module Domain = Block.Node.Set

    module MakeHelpers : functor (Block : Block) ->
      DominatorHelpers.S with module Block := Block
  end
end
