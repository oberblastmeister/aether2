type t = private int [@@deriving compare, equal, hash, sexp]

val to_int : t -> int
val next : t -> t
val of_int : int -> t
val of_global_unique : unit -> t
