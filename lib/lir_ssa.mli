open Lir_instr

val convert_naive_ssa : Value.t Function.t -> Value.t Function.t
val convert_ssa : Value.t Program.t -> Value.t Program.t
