open O
open Ast

module Allocation : sig
  type t [@@deriving sexp_of]

  val used_registers : t -> Mach_reg.t F.Iter.t
end

val alloc_function : VReg.t Function.t -> Allocation.t

val apply_allocation_function
  :  allocation:Allocation.t
  -> stack_builder:Stack_builder.t
  -> VReg.t Function.t
  -> AReg.t Function.t
