open Core

module Enum = struct
  module type S = sig
    type t [@@deriving sexp_of]

    val min : int
    val max : int
    val to_enum : t -> int
    val of_enum : int -> t option
  end
end

module type Gen_S = sig
  type ('a, 'b, 'c) t
  type ('a, 'b, 'c) elt

  val create : unit -> ('a, 'b, 'c) t
  val add : ('a, 'b, 'c) t -> ('a, 'b, 'c) elt -> unit
  val remove : ('a, 'b, 'c) t -> ('a, 'b, 'c) elt -> unit
  val mem : ('a, 'b, 'c) t -> ('a, 'b, 'c) elt -> bool
  val iter : ('a, 'b, 'c) t -> f:(('a, 'b, 'c) elt -> unit) -> unit
end

module type S = sig
  type t [@@deriving sexp_of]
  type elt

  include Gen_S with type ('a, 'b, 'c) t := t and type ('a, 'b, 'c) elt := elt
end

module type Intf = sig
  type 'a t

  val negate : 'a t -> unit

  module Make (T : Enum.S) : S with type t = T.t t and type elt = T.t
end
