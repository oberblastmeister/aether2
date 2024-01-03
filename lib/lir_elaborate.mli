open O

val elaborate_single :
  Lir_instr.Name.t Lir_instr.Function.t' -> Lir_instr.Function.t Or_error.t

val elaborate :
  Lir_instr.Name.t Lir_instr.Function.t' list ->
  Lir_instr.Function.t list Or_error.t
