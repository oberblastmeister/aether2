open! O

module type S = sig
  type key [@@deriving equal, compare, hash, sexp]
  type t = key Raw_id.t [@@deriving equal, compare, hash, sexp]

  module Gen : sig
    type id = t
    type t

    val create : unit -> t
    val of_id : id -> t
    val to_id : t -> id
    val next : t -> id
  end

  val of_int : int -> t
  val to_int : t -> int
  val of_raw : t -> t
  val to_raw : t -> t
  val next : t -> t
  val of_global_unique : unit -> t

  include Comparable.S with type t := t
end

module type Intf = sig
  module type S = S

  module Make : () -> S
end
