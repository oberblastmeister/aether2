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
