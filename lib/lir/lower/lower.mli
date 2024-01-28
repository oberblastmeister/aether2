open O
module Lir = Types
module Tir = Tir

val run : Vir.Program.t -> Tir.Program.t
