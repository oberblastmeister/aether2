open! O
open Utils.Instr_types

module Idoms : sig
  type t [@@deriving sexp_of]

  val find : t -> Label.t -> Label.t option
end

module Frontier : sig
  type t [@@deriving sexp_of]

  val find : t -> Label.t -> Label.t F.Iter.t
end

val get_idoms : ?node_length:int -> start:Label.t -> Label.t Data.Graph.double -> Idoms.t
val frontier_of_idoms : Idoms.t -> Label.t Data.Graph.double -> Frontier.t
