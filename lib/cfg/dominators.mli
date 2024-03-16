open! O
open Utils.Instr_types

(* Maps labels to their immediate dominator *)
module Idoms : sig
  type t [@@deriving sexp_of]

  val find : t -> Label.t -> Label.t option
  val dominates : t -> Label.t -> Label.t -> bool
end

(* A tree of labels to other labels that are immediately dominated *)
module Domtree : sig
  type t [@@deriving sexp_of]

  val children : t -> Label.t -> Label.t list
  val of_idoms : Idoms.t -> t
end

module Frontier : sig
  type t [@@deriving sexp_of]

  val find : t -> Label.t -> Label.t F.Iter.t
end

val get_idoms : ?node_length:int -> start:Label.t -> Label.t Data.Graph.double -> Idoms.t
val frontier_of_idoms : Idoms.t -> Label.t Data.Graph.double -> Frontier.t
