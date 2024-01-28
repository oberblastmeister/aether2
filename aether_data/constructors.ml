open! Core

module Hashtbl_ext = struct
  include Hashtbl
  include Higher_kinded.Make2 (Hashtbl)
end

module type Set = sig
  type a
  type t

  val create : ?size:int -> unit -> t
  val add : t -> a -> unit
  val mem : t -> a -> bool
end

module type Map = sig
  type k
  type h
  type 'a t = (k -> 'a -> h) Higher_kinded.t

  val create : ?size:int -> unit -> 'a t
  val set : 'a t -> key:k -> data:'a -> unit
  val change : 'a t -> k -> f:('a option -> 'a option) -> unit
  val find : 'a t -> k -> 'a option
end

type ('a, 't) set = (module Set with type a = 'a and type t = 't)
type 'a some_set = (module Set with type a = 'a)
type 'k some_map = (module Map with type k = 'k)
type ('k, 'h) map = (module Map with type h = 'h and type k = 'k)

let hash_set (type a) (module Key : Hashtbl.Key with type t = a) =
  (module struct
    type a = Key.t
    type t = Key.t Hash_set.t

    let create ?size () = Hash_set.create (module Key) ?size
    let add t a = Hash_set.add t a
    let mem t a = Hash_set.mem t a
  end : Set
    with type a = a
     and type t = Key.t Hash_set.t)
;;

let to_some_set (type a t) (module Set : Set with type a = a and type t = t) : a some_set =
  (module Set)
;;

let some_hashset key = hash_set key |> to_some_set

let to_some_map (type k h) (module Map : Map with type k = k and type h = h) : k some_map =
  (module Map)
;;

let hashtbl (type k) (module Key : Hashtbl.Key with type t = k) =
  (module struct
    type k = Key.t
    type h = Hashtbl_ext.higher_kinded
    type 'a t = (k -> 'a -> h) Higher_kinded.t

    let create ?size () = Hashtbl.create ?size (module Key) |> Hashtbl_ext.inject
    let set t ~key ~data = Hashtbl.set (Hashtbl_ext.project t) ~key ~data
    let change t key ~f = Hashtbl.change (Hashtbl_ext.project t) key ~f
    let find t key = Hashtbl.find (Hashtbl_ext.project t) key
  end : Map
    with type h = Hashtbl_ext.higher_kinded
     and type k = k)
;;

let some_hashtbl key = hashtbl key |> to_some_map

module Result : sig
  type ('a, 'e) t = ('a, 'e) result

  include Higher_kinded.S2 with type ('a, 'e) t := ('a, 'e) t
end = struct
  type ('a, 'e) t = ('a, 'e) result

  include Higher_kinded.Make2 (Base.Result)
end
