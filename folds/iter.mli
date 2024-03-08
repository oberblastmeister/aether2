type 'a t = f:('a -> unit) -> unit

val empty : 'a t
val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val snoc : 'a t -> 'a -> 'a t
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val sum : int t -> int
val filter : 'a t -> f:('a -> bool) -> 'a t
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
val enumerate : 'a t -> (int * 'a) t
val map : 'a t -> f:('a -> 'b) -> 'b t
val length : 'a t -> int
val unfoldr : init:'s -> f:('s -> ('a * 's) option) -> 'a t
val to_list : 'a t -> 'a list
val to_array : 'a t -> 'a array
val find_map : 'a t -> f:('a -> 'b option) -> 'b option
val find : 'a t -> f:('a -> bool) -> 'a option
val exists : 'a t -> f:('a -> bool) -> bool
val int_range : start:int -> stop:int -> int t
val rev : 'a t -> 'a t

module Infix : sig
  val ( -- ) : int -> int -> int t
end

val ( -- ) : int -> int -> int t

module Private : sig
  module MList : sig
    type 'a t

    val iter_rev : 'a t -> f:('a -> unit) -> unit
  end

  val to_mlist : 'a t -> 'a MList.t
end
