open O

type ann = Line | IndentLine [@@deriving equal, compare, sexp]

type t = List of t list | Atom of string | Ann of ann
[@@deriving equal, compare, sexp]

module type Sexp_print = sig
  type nonrec t = t [@@deriving equal, compare, sexp]

  val print : t -> string
end
