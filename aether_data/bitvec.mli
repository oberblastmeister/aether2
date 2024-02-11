open Core
module F = Folds

type t [@@deriving equal, sexp]

module C : Indexed_container.S0_with_creators with type t := t

val length : t -> int
val empty : unit -> t
val create : size:int -> bool -> t
val copy : t -> t
val set : t -> int -> unit
val unset : t -> int -> unit
val get : t -> int -> bool
val union_into : into:t -> t -> unit
val inter_into : into:t -> t -> unit
val union : t -> t -> t
val inter : t -> t -> t
val diff_into : into:t -> t -> unit
val diff : t -> t -> t
val negate_self : t -> unit
val negate : t -> t
val iteri : t -> f:(int -> bool -> unit) -> unit
val iter : t -> f:(bool -> unit) -> unit
val to_iter : t -> bool F.Iter.t
val init : int -> f:(int -> bool) -> t
val of_array : bool array -> t
val to_array : t -> bool array
val of_list : bool list -> t
val to_list : t -> bool list
val fold : t -> init:'a -> f:('a -> bool -> 'a) -> 'a
