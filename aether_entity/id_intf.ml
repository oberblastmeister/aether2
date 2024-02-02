open! Core

module type Key_type = sig
  type key
end

module type S' = sig
  type key [@@deriving equal, compare, hash, sexp]
  type 'k t'
  type nonrec t = key t' [@@deriving equal, compare, hash, sexp]

  val of_int : int -> t
  val to_int : t -> int
  val of_raw : Raw_id.t -> t
  val to_raw : t -> Raw_id.t
  val next : t -> t
  val of_global_unique : unit -> t

  include Base.Comparable.S with type t := t
end

module type Intf = sig
  type 'k t = private Raw_id.t [@@deriving equal, compare, hash, sexp]

  val of_int : int -> 'k t
  val to_int : 'k t -> int
  val of_raw : Raw_id.t -> 'k t
  val to_raw : 'k t -> Raw_id.t
  val next : 'k t -> 'k t
  val of_global_unique : unit -> 'k t

  module type S = S' with type 'k t' := 'k t
  module type Key_type = Key_type

  module Make : () -> S
end
