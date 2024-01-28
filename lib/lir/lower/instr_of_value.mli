open O

type t

open Types

val create : Vir.Some_instr.t F.Iter.t -> t
val find_data : t -> Value.t -> (int * Vir.Instr.t) option
val find : t -> Value.t -> Vir.Instr.t option
val find_index : t -> Value.t -> int option
