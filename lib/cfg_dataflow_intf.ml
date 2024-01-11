open O
open Instr_types

module type InstrLike = sig
  type t [@@deriving sexp_of]

  module Value : sig
    type t [@@deriving equal, compare, sexp, hash]
  end

  val uses : t -> Value.t list
  val defs : t -> Value.t list
end

module type BlockLike = sig
  type t [@@deriving sexp_of]
  type instr

  val jumps : t -> Label.t list
  val fold_instrs_forward : t -> init:'a -> f:('a -> instr -> 'a) -> 'a
  val fold_instrs_backward : t -> init:'a -> f:('a -> instr -> 'a) -> 'a
end

module Direction = struct
  type t =
    | Forward
    | Backward
end

module type InstrTransfer = sig
  type instr [@@deriving sexp_of]
  type domain [@@deriving sexp_of]

  val transfer : instr -> domain -> domain
  val changed : current_fact:domain -> new_fact:domain -> bool
  val empty : domain
  val combine : domain list -> domain
  val direction : Direction.t
end

module type BlockTransfer = sig
  type block
  type domain [@@deriving sexp_of]

  val transfer
    :  Label.t
    -> block
    -> other_facts:domain list
    -> current_fact:domain
    -> domain Option.t

  val empty : domain
  val direction : Direction.t
end

module type ConvertedInstrTransfer = sig
  include BlockTransfer
  module InstrTransfer : InstrTransfer
end

module type BlockRewriteTransfer = sig
  type block
  type domain [@@deriving sexp_of]
end

module type Dataflow = sig
  module Block : BlockLike

  module MakeRun : functor (Transfer : BlockTransfer with type block = Block.t) -> sig
    val run : Block.t Cfg_graph.Graph.t -> Transfer.domain list
  end
end

module type Cfg_dataflow = sig
  module InstrToBlockTransfer : functor
      (Block : BlockLike)
      (Transfer : InstrTransfer with type instr = Block.instr)
      ->
    ConvertedInstrTransfer
    with type block = Block.t
     and type domain = Transfer.domain
     and module InstrTransfer = Transfer
end
