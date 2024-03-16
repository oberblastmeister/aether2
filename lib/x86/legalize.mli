open Ast

(* val legalize_minstr
   :  'r MInstr.t
   -> force_same:(size:Size.t -> dst:'r Operand.t -> src:'r Operand.t -> 'r Operand.t)
   -> force_register:(size:Size.t -> 'r Operand.t -> 'r)
   -> 'r MInstr.t *)

(* func index is used for the local labels produced *)
val legalize_function : func_index:int -> AReg.t Function.t -> AReg.t Flat.Program.t
