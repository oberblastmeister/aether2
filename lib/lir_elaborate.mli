open O
open Lir_instr

val elaborate_single : Name.t Function.t' -> Function.t Or_error.t
val elaborate : Name.t Function.t' list -> Function.t list Or_error.t
