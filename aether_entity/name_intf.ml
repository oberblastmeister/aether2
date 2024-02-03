open! Core

type 'k t =
  { name : string [@equal.skip] [@compare.skip] [@hash.skip]
  ; id : 'k Id.t
  }
[@@deriving equal, compare, hash, sexp]

let to_dotted_string (name_id : _ t) =
  name_id.name ^ "." ^ string_of_int (Raw_id.to_int @@ Id.to_raw name_id.id)
;;

(* TODO: fix t_of_sexp *)
let sexp_of_t _f t = Sexp.Atom (to_dotted_string t)

module type S = sig
  module Id : Id.S

  type key = Id.key [@@deriving equal, compare, hash, sexp]
  type nonrec t = Id.key t [@@deriving equal, compare, hash, sexp]

  val create : string -> Id.t -> t
  val of_string_global_unique : string -> t
  val to_id : t -> Id.t
  val to_raw : t -> Raw_id.t

  include Comparable.S with type t := t
  include To_id.S with type t := t and type key := key
end

module type Intf = sig
  type nonrec 'k t = 'k t [@@deriving equal, compare, hash, sexp]

  module type S = S

  module Make : () -> S

  val to_dotted_string : 'k t -> string
end
