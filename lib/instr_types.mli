open! O

module Control : sig
  type e = E
  type o = O
  type c = C
end

module SControl : sig
  type 'a t =
    | SO : Control.o t
    | SC : Control.c t
    | SE : Control.e t
end

module UniqueName : sig
  type t =
    { name : string [@equal.ignore] [@compare.ignore] [@hash.ignore]
    ; unique : int
    }
  [@@deriving sexp, equal, compare, hash]

  val to_string : t -> string
end

module Name : sig
  type t =
    | Name of string
    | Unique of UniqueName.t
  [@@deriving sexp, equal, compare, hash]

  val to_string : t -> string
  val of_string : string -> t

  module Hashtbl : Hashtbl.S with type key = t
  module Hash_set : Hash_set.S with type elt = t
  include Comparable.S with type t := t
end

module Label : Name_id.S
