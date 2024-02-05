module Tir = Lower.Tir

val lower : Tir.Program.t -> X86.Types.VReg.t X86.Types.Program.t
