open Core

type 'a enum =
  { to_enum : 'a -> int
  ; of_enum_exn : int -> 'a
  ; sexp_of : 'a -> Sexp.t
  ; max : int
  }

module Enum = struct
  module type S = sig
    type t [@@deriving sexp_of]

    val max : int
    val to_enum : t -> int
    val of_enum : int -> t option
  end
end

module type Gen_S = sig
  type ('a, 'b, 'c) t
  type ('a, 'b, 'c) elt

  val enum : ('a, 'b, 'c) elt enum
  val create : unit -> ('a, 'b, 'c) t
  val add : ('a, 'b, 'c) t -> ('a, 'b, 'c) elt -> unit
  val remove : ('a, 'b, 'c) t -> ('a, 'b, 'c) elt -> unit
  val mem : ('a, 'b, 'c) t -> ('a, 'b, 'c) elt -> bool
  val iter : ('a, 'b, 'c) t -> f:(('a, 'b, 'c) elt -> unit) -> unit
end

module type S = sig
  type t [@@deriving sexp_of]
  type elt

  include Gen_S with type ('a, 'b, 'c) t := t and type ('a, 'b, 'c) elt := elt
end

module type Intf = sig
  type nonrec 'a enum = 'a enum
  type 'a t

  val create : enum:'a enum -> unit -> 'a t
  val add : enum:'a enum -> 'a t -> 'a -> unit
  val remove : enum:'a enum -> 'a t -> 'a -> unit
  val mem : enum:'a enum -> 'a t -> 'a -> bool
  val iter : enum:'a enum -> 'a t -> f:('a -> unit) -> unit
  val negate : 'a t -> unit
  val sexp_of_t_with : enum:'a enum -> 'a t -> Sexp.t
  val count : 'a t -> int

  module Enum : sig
    module type S = Enum.S
  end

  module Make_enum (T : Enum.S) : sig
    val enum : T.t enum
  end

  module Make (T : Enum.S) : S with type t = T.t t and type elt = T.t
end
