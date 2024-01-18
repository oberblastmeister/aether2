type 'k t [@@deriving compare, equal, hash, sexp]

val to_int : 'k t -> int
val of_string_int : string -> int -> 'k t
val of_string_global_unique : string -> 'k t
