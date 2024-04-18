open Ast

(* func index is used for the local labels produced *)
val legalize_function : func_index:int -> AReg.t Function.t -> AReg.t Flat.Program.t
