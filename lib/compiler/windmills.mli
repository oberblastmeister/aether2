open O
open Utils.Instr_types

module Move : sig
  type 'a t [@@deriving sexp_of]

  val create : dst:'a -> src:'a -> 'a t
end

val convert
  :  move:(dst:'a -> src:'a -> 'b)
  -> get_name:('a -> Name.t)
  -> scratch:('a -> 'a)
  -> 'a Move.t array
  -> ('b, [< Core.read ]) Vec.t * bool
