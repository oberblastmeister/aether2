open O
open Utils.Instr_types
open Ast

type t =
  { stack_slot_of_mach_reg : (Mach_reg.t, Stack_slot.t) Hashtbl.t
  ; stack_slot_of_name : (Name.t, Stack_slot.t) Hashtbl.t
  ; mutable stack_instrs : Stack_instr.t list
  ; mutable unique_stack_slot : Stack_slot.Id.t
  }

let create unique_stack_slot =
  { stack_slot_of_mach_reg = Hashtbl.create (module Mach_reg)
  ; stack_slot_of_name = Hashtbl.create (module Name)
  ; stack_instrs = []
  ; unique_stack_slot
  }
;;

let fresh_stack_slot t s =
  let stack_slot = Stack_slot.create s t.unique_stack_slot in
  t.unique_stack_slot <- Stack_slot.Id.next t.unique_stack_slot;
  t.stack_instrs <- ReserveLocal { stack_slot; size = 8l } :: t.stack_instrs;
  stack_slot
;;

let stack_slot_of_name t name =
  match Hashtbl.find t.stack_slot_of_name name with
  | Some stack_slot -> stack_slot
  | None ->
    let stack_slot = fresh_stack_slot t name.name in
    Hashtbl.add_exn t.stack_slot_of_name ~key:name ~data:stack_slot;
    stack_slot
;;

let stack_slot_of_mach_reg cx reg =
  match Hashtbl.find cx.stack_slot_of_mach_reg reg with
  | Some stack_slot -> stack_slot
  | None ->
    let stack_slot = fresh_stack_slot cx "spill_mach_reg" in
    Hashtbl.add_exn cx.stack_slot_of_mach_reg ~key:reg ~data:stack_slot;
    cx.stack_instrs
    <- ReserveLocal
         { stack_slot
         ; size =
             Int.to_int32_exn
               (Size.to_byte_size (Reg_class.max_size (Reg_class.of_mach_reg reg)))
         }
       :: cx.stack_instrs;
    stack_slot
;;

let get_stack_instrs t = t.stack_instrs
