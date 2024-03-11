open! Core

module type Key_type = sig
  type key
end

type 'k key_proxy = (module Key_type with type key = 'k)

(* this is S' because we want the t to be equal to the abstract/private type which hasn't been defined yet *)
(* we don't want to put it inside of the Intf because then it wouldn't be shared the implementation file *)
(* so we just put it here *)
(* make sure to substitute the t' whenever you define the abstract/private type *)
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

  (* this is the generic api *)
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
