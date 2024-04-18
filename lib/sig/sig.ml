open O

module type T = sig
  type t
end

module type Set = sig
  type t [@@deriving sexp_of]
  type elt

  val create : unit -> t
  val mem : t -> elt -> bool
  val add : t -> elt -> unit
end

module type Map = sig
  type t [@@deriving sexp_of]
  type elt

  val create : unit -> t
  val mem : t -> elt -> bool
  val add : t -> elt -> unit
end

module ListSet = struct
  type t = int list ref [@@deriving sexp_of]
  type elt = int

  let create () = ref []
  let mem t x = List.mem !t x ~equal:( = )
  let add t x = t := x :: !t
  let index = List.findi_exn
end

module HashSet = struct
  type t = int Core.Hash_set.t [@@deriving sexp_of]
  type elt = int

  let create () = Core.Hash_set.create (module Core.Int)
  let mem = Core.Hash_set.mem
  let add = Core.Hash_set.add
end

module Set : Set with type elt = int = ListSet
