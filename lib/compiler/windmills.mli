open O
open Utils.Instr_types

module Move : sig
  type 'a t =
    { dst : 'a
    ; src : 'a
    }
  [@@deriving sexp_of]

  val create : dst:'a -> src:'a -> 'a t
end

val convert
  :  get_name:('a -> Name.t)
  -> scratch:('a -> 'a)
  -> 'a Move.t list
  -> 'a Move.t list * bool
(* -> ('b, [< Core.read ]) Vec.t * bool *)
