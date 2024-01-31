open Core

module type Intf = sig
  type ('a, 'cmp) t

  val create : ?size:int -> unit -> ('a, 'cmp) t
  val push : cmp:('a, 'cmp) Comparator.t -> ('a, 'cmp) t -> 'a -> unit
  val pop : cmp:('a, 'cmp) Comparator.t -> ('a, 'cmp) t -> 'a option
  val get : ('a, _) t -> int -> 'a
  val length : ('a, _) t -> int

  module Indexed : sig
    type ('a, 'cmp) t

    module Make_with_comparator (C : Comparator.S) : sig
      type nonrec t = (C.t, C.comparator_witness) t

      val pop : t -> C.t option
      val push : t -> C.t -> unit
      val remove : t -> int -> unit
    end
  end
end
