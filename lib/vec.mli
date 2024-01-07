open O

type ('a, -'perms) t

val create : ?capacity:int -> unit -> ('a, [< _ perms ]) t
val get : ('a, [> read ]) t -> int -> 'a
val pop : ('a, [> read_write ]) t -> 'a option
val pop_exn : ('a, [> read_write ]) t -> 'a
val push : ('a, [> write ]) t -> 'a -> unit
val rev_inplace : ('a, [> read_write ]) t -> unit
val to_array : ('a, [> read ]) t -> 'a array
val of_array : 'a array -> ('a, [< _ perms ]) t
val to_list : ('a, [> read ]) t -> 'a list
val of_list : 'a list -> ('a, [< _ perms ]) t
val fold : ('a, [> read ]) t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val fold_right : ('a, [> read ]) t -> init:'b -> f:('a -> 'b -> 'b) -> 'b
val sexp_of_t : ('a -> Sexp.t) -> ('a, [> read ]) t -> Sexp.t
val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> ('a, [< _ perms ]) t
