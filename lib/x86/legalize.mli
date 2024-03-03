open Types

(* val legalize_minstr
   :  'r MInstr.t
   -> force_same:(size:Size.t -> dst:'r Operand.t -> src:'r Operand.t -> 'r Operand.t)
   -> force_register:(size:Size.t -> 'r Operand.t -> 'r)
   -> 'r MInstr.t *)

val legalize_function : AReg.t Function.t -> AReg.t Flat.Program.t
