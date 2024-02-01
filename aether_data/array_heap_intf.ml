open Core

module type Key = sig
  type t

  include Comparator.S with type t := t
  include Sexpable.S with type t := t
end

module type Intf = sig
  type ('a, 'cmp) t

  val create : ?size:int -> unit -> ('a, 'cmp) t
  val push : cmp:('a, 'cmp) Comparator.t -> ('a, 'cmp) t -> 'a -> unit
  val pop : cmp:('a, 'cmp) Comparator.t -> ('a, 'cmp) t -> 'a option
  val get : ('a, _) t -> int -> 'a
  val length : ('a, _) t -> int

  module Indexed : sig
    type ('a, 'cmp) t

    module Make_with_comparator (C : Key) : sig
      type nonrec t = (C.t, C.comparator_witness) t [@@deriving sexp_of]

      val create : ?size:int -> unit -> t
      val invariant : t -> unit
      val pop : t -> C.t option
      val set : t -> key:int -> data:C.t -> unit
      val modify : t -> key:int -> f:(C.t -> C.t) -> unit
      val remove : t -> int -> C.t option
    end
  end
end
