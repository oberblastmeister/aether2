open O
open Lir_instr

val parse : string -> Name.t Program.t' Or_error.t
