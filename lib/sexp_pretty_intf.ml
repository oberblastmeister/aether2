open O

type ann = Line | IndentLine [@@deriving equal, compare, sexp]

module T = struct
  type t = List of t list | Atom of string | Ann of ann
  [@@deriving equal, compare, sexp, variants]
end

include T

module type Sexp_pretty = sig
  include module type of T

  val to_string : t -> string
end
