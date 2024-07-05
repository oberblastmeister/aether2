module Tir = Lower.Tir

val lower : Tir.Module.t -> X86.Ast.VReg.t X86.Ast.Program.t
