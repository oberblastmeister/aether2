open! Core
module F = Folds

module Raw : sig
  type 'a t [@@deriving equal, compare, hash, sexp, quickcheck]

  val length : _ t -> int
  val create : ?size:int -> unit -> 'a t
  val pop : 'a t -> 'a option
  val pop_exn : 'a t -> 'a
  val push : 'a t -> 'a -> unit
  val unsafe_pop : 'a t -> 'a
  val unsafe_push : 'a t -> 'a -> unit
  val get : 'a t -> int -> 'a
  val unsafe_get : 'a t -> int -> 'a
  val unsafe_set : 'a t -> int -> 'a -> unit
  val unsafe_swap : 'a t -> int -> int -> unit
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val of_array : 'a array -> 'a t
  val to_array : 'a t -> 'a array
  val iter : 'a t -> f:('a -> unit) -> unit
  val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val fold_right : 'a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b
end

type ('a, -'perms) t [@@deriving equal, compare, hash, sexp]

val cons : 'a -> ('a, [> read ]) t -> ('a, [< _ perms ]) t
val create : ?size:int -> unit -> ('a, [< _ perms ]) t
val get : ('a, [> read ]) t -> int -> 'a
val set : ('a, [> write ]) t -> int -> 'a -> unit
val get_opt : ('a, [> read ]) t -> int -> 'a option
val first : ('a, [> read ]) t -> 'a option
val last : ('a, [> read ]) t -> 'a option
val pop : ('a, [> read_write ]) t -> 'a option
val pop_exn : ('a, [> read_write ]) t -> 'a
val push : ('a, [> write ]) t -> 'a -> unit
val rev_inplace : ('a, [> read_write ]) t -> unit
val to_array : ('a, [> read ]) t -> 'a array
val of_array : 'a array -> ('a, [< _ perms ]) t
val to_list : ('a, [> read ]) t -> 'a list
val length : ('a, _) t -> int
val of_list : 'a list -> ('a, [< _ perms ]) t
val of_iter : 'a F.Iter.t -> ('a, [< _ perms ]) t
val iter : ('a, [> read ]) t -> f:('a -> unit) -> unit
val iter_rev : ('a, [> read ]) t -> f:('a -> unit) -> unit
val to_iter : ('a, [> read ]) t -> 'a F.Iter.t
val to_iter_rev : ('a, [> read ]) t -> 'a F.Iter.t
val iteri : ('a, [> read ]) t -> f:(int -> 'a -> unit) -> unit
val map : ('a, [> read_write ]) t -> f:('a -> 'a) -> unit
val map_copy : ('a, [> read ]) t -> f:('a -> 'b) -> ('b, [< _ perms ]) t
val fold : ('a, [> read ]) t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val fold_right : ('a, [> read ]) t -> init:'b -> f:('a -> 'b -> 'b) -> 'b
val freeze : ('a, [> read ]) t -> ('a, [< read ]) t
val copy : ('a, [> read ]) t -> ('a, [< _ perms ]) t
val copy_exact : ?size:int -> ('a, [> read ]) t -> ('a, [< _ perms ]) t
val shrink_to_fit : ('a, [> read_write ]) t -> unit
val of_raw : 'a Raw.t -> ('a, [< _ perms ]) t
val to_raw : ('a, [> read_write ]) t -> 'a Raw.t
val clear : ('a, [> read_write ]) t -> unit
