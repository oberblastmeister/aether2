open Utils.Instr_types

type t

val create : Stack_slot.Id.t -> t
val fresh_stack_slot : t -> string -> Stack_slot.t
val stack_slot_of_name : t -> Name.t -> Stack_slot.t
val stack_slot_of_mach_reg : t -> Mach_reg.t -> Stack_slot.t
val get_stack_instrs : t -> Ast.Stack_instr.t list
