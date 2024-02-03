open O
open Utils.Instr_types

type 'a move =
  { dst : 'a
  ; src : 'a
  }

val convert
  :  move:(dst:'a -> src:'a -> 'b)
  -> get_name:('a -> Name.t)
  -> scratch:('a -> 'a)
  -> 'a move array
  -> ('b, [< Core.read ]) Vec.t * bool
