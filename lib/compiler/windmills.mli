open! O

module Move : sig
  type ('a, 'o) t =
    { dst : 'o
    ; src : 'o
    ; ann : 'a
    }
  [@@deriving sexp_of]

  val create : dst:'o -> src:'o -> ann:'a -> ('a, 'o) t
end

val convert
  :  eq:('o -> 'o -> bool)
  -> scratch:('o -> 'o)
  -> ('a, 'o) Move.t list
  -> ('a, 'o) Move.t list * bool
