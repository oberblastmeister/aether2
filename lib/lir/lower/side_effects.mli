open! O
open Types

module Color : sig
  type t [@@deriving sexp_of]

  val is_before : t -> t -> bool
end

module Color_of_index : sig
  type t [@@deriving sexp_of]

  val get : t -> int -> Color.t
end

val color : Vir.Graph.t -> Color_of_index.t
