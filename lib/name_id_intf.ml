open! O

type 'k t =
  { name : string [@equal.skip] [@compare.skip] [@hash.skip]
  ; id : 'k Raw_id.t
  }
[@@deriving equal, compare, hash, sexp]

module type S = sig
  module Id : Id.S

  type nonrec t = Id.key t [@@deriving equal, compare, hash, sexp]

  val create : string -> Id.t -> t
  val of_string_global_unique : string -> t

  include Comparable.S with type t := t
end

module type Intf = sig
  type nonrec 'k t = 'k t [@@deriving equal, compare, hash, sexp]

  module type S = S

  module Make : () -> S

  val to_dotted_string : 'k t -> string
end
