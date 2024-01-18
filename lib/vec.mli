open! O

module Raw : sig
  type 'a t [@@deriving equal, compare, hash, sexp, quickcheck]
end

type ('a, -'perms) t [@@deriving equal, compare, hash, sexp]

val create : ?capacity:int -> unit -> ('a, [< _ perms ]) t
val get : ('a, [> read ]) t -> int -> 'a
val pop : ('a, [> read_write ]) t -> 'a option
val pop_exn : ('a, [> read_write ]) t -> 'a
val push : ('a, [> write ]) t -> 'a -> unit
val rev_inplace : ('a, [> read_write ]) t -> unit
val to_array : ('a, [> read ]) t -> 'a array
val of_array : 'a array -> ('a, [< _ perms ]) t
val to_list : ('a, [> read ]) t -> 'a list
val length : ('a, [> read ]) t -> int
val of_list : 'a list -> ('a, [< _ perms ]) t
val iter : ('a, [> read ]) t -> f:('a -> unit) -> unit
val iteri : ('a, [> read ]) t -> f:(int -> 'a -> unit) -> unit
val fold : ('a, [> read ]) t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val fold_right : ('a, [> read ]) t -> init:'b -> f:('a -> 'b -> 'b) -> 'b
val freeze : ('a, [> read ]) t -> ('a, [> read ]) t
val copy : ('a, [> read ]) t -> ('a, [< _ perms ]) t
