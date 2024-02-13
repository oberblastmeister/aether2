open! O

module Move : sig
  type 'a t =
    { dst : 'a
    ; src : 'a
    }
  [@@deriving sexp_of]

  val create : dst:'a -> src:'a -> 'a t
end

val convert
  :  eq:('a -> 'a -> bool)
  -> scratch:('a -> 'a)
  -> 'a Move.t list
  -> 'a Move.t list * bool
(* -> ('b, [< Core.read ]) Vec.t * bool *)
