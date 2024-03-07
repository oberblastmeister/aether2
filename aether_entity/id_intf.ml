open! Core

module type Key_type = sig
  type key
end

type 'k key_proxy = (module Key_type with type key = 'k)

module type S' = sig
  type key [@@deriving equal, compare, hash, sexp]
  type 'k t'
  type nonrec t = key t' [@@deriving equal, compare, hash, sexp]

  val initial : t
  val of_int : int -> t
  val to_int : t -> int
  val next : t -> t
  val prev : t -> t
  val of_global_unique : unit -> t

  include Base.Comparable.S with type t := t
end

module type Intf = sig
  type 'k t = private int [@@deriving equal, compare, hash, sexp]

  val initial : 'k t
  val of_int : int -> 'k t
  val to_int : 'k t -> int
  val next : 'k t -> 'k t
  val prev : 'k t -> 'k t
  val of_global_unique : unit -> 'k t

  module type S = S' with type 'k t' := 'k t
  module type Key_type = Key_type

  type nonrec 'k key_proxy = 'k key_proxy

  module Make : () -> S
end
