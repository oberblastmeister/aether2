open! O

type 'a t = T of ('a * 'a list) [@@deriving equal, compare, hash, sexp]

val ( @ ) : 'a t -> 'a t -> 'a t
val ( |: ) : 'a -> 'a t -> 'a t
val singleton : 'a -> 'a t
val of_list_exn : 'a list -> 'a t
val of_list : 'a list -> 'a t option
val to_list : 'a t -> 'a list
val hd : 'a t -> 'a
val tl : 'a t -> 'a list
val all_equal : ('a -> 'a -> bool) -> 'a t -> bool
val fold_map : f:('a -> 'b) -> combine:('b -> 'b -> 'b) -> 'a t -> 'b
