open O

type t = { start : int; stop : int }
[@@deriving sexp, equal, compare, sexp, hash, accessors]

val empty : t
val single : int -> t
val combine : t -> t -> t
