open O
open Lir_instr

val elaborate : Name.t Program.t' -> Program.t Or_error.t
