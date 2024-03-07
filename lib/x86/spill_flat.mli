open O
open Types
open Flat

val lower_function
  :  Name.Id.t
  -> AReg.t Flat.Program.t
  -> MReg.t Flat.Program.t * (Name.t * int32, read) Vec.t
