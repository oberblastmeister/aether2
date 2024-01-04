open O
open Lir_instr

val parse : string -> Name.t Function.t' list Or_error.t
