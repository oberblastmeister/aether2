open! O
open Ast
open Utils.Instr_types
module Interference = Compiler.Reg_alloc.Interference

module Resolve_stack : sig
  val resolve_function : Stack_layout.t -> MReg.t Flat.Program.t -> MReg.t Flat.Program.t
end = struct
  let resolve_imm stack imm =
    match imm with
    | Imm.Int i -> Imm.Int i
    | Stack (End i) -> Int (Stack_layout.end_offset stack i)
    | Stack (Local name) -> Int (Stack_layout.local_offset stack name)
    | Stack (Start i) -> Int (Stack_layout.start_offset stack i)
  ;;

  let resolve_address stack address =
    match address with
    | Address.Imm { offset; scale } ->
      Address.Imm { offset = resolve_imm stack offset; scale }
    | Complex { base; index; offset } ->
      Complex { base; index; offset = resolve_imm stack offset }
  ;;

  let resolve_operand stack operand =
    match operand with
    | Operand.Imm imm -> Operand.Imm (resolve_imm stack imm)
    | Reg reg -> Reg reg
    | Mem mem -> Mem (Mem.map_addr mem ~f:(resolve_address stack))
  ;;

  let resolve_instr stack (minstr : _ Flat.Instr.t) : _ Flat.Instr.t =
    let resolve_operand = resolve_operand stack in
    match minstr with
    | Mov { dst; src } -> Mov { dst = resolve_operand dst; src = resolve_operand src }
    | MovAbs { dst; imm } -> MovAbs { dst = resolve_operand dst; imm }
    | Lea { dst; src } -> Lea { dst = resolve_operand dst; src = resolve_operand src }
    | Add { dst; src } -> Add { dst = resolve_operand dst; src = resolve_operand src }
    | Sub { dst; src } -> Sub { dst = resolve_operand dst; src = resolve_operand src }
    | Push { src } -> Push { src = resolve_operand src }
    | Pop { dst } -> Pop { dst = resolve_operand dst }
    | Cmp { src1; src2 } ->
      Cmp { src1 = resolve_operand src1; src2 = resolve_operand src2 }
    | Test { src1; src2; _ } ->
      Test { src1 = resolve_operand src1; src2 = resolve_operand src2 }
    | Set { dst; cond } -> Set { dst = resolve_operand dst; cond }
    | Call p -> Call p
    | J { cond; src } -> J { cond; src }
    | Jmp { src } -> Jmp { src }
    | Ret -> Ret
  ;;

  let resolve_function stack_layout fn =
    Flat.Program.map_instrs fn ~f:(resolve_instr stack_layout)
  ;;
end

let create_prologue stack_layout =
  [ Flat.Instr.Sub
      { dst = Reg (MReg.create Q RSP); src = Imm (Int (Stack_layout.size stack_layout)) }
  ]
;;

let create_epilogue stack_layout =
  [ Flat.Instr.Add
      { dst = Reg (MReg.create Q RSP); src = Imm (Int (Stack_layout.size stack_layout)) }
  ; Ret
  ]
;;

let run_function ~func_index (fn : _ Function.t) =
  let stack_builder = Stack_builder.create fn.unique_stack_slot in
  let allocation = Reg_alloc.alloc_function fn in
  let callee_saved =
    Reg_alloc.Allocation.used_registers allocation
    |> F.Iter.filter ~f:(fun reg ->
      List.mem ~equal:Mach_reg.equal Mach_reg.callee_saved reg)
    |> F.Iter.map ~f:(fun reg ->
      MReg.create Q reg, Stack_builder.fresh_stack_slot stack_builder "spill_callee_saved")
    |> F.Iter.to_list
  in
  [%log.global.debug
    (allocation : Reg_alloc.Allocation.t) (callee_saved : (MReg.t * Stack_slot.t) list)];
  let fn = Reg_alloc.apply_allocation_function ~allocation ~stack_builder fn in
  let fn = Remove_ssa.remove_ssa fn in
  let flat = Legalize.legalize_function ~func_index fn in
  let flat = Spill_flat.lower_function stack_builder flat in
  let stack_instrs = fn.stack_instrs @ Stack_builder.get_stack_instrs stack_builder in
  let stack_layout = Stack_layout.create stack_instrs in
  let flat =
    let flat' = Vec.create ~size:(Vec.length flat + 10) () in
    (* spill callee saved *)
    List.iter callee_saved ~f:(fun (reg, slot) ->
      Vec.push
        flat'
        (Flat.Line.Instr
           (Flat.Instr.Mov { dst = Operand.stack_local Q slot; src = Operand.Reg reg }));
      ());
    Vec.append_into ~into:flat' flat;
    (* reload callee saved *)
    List.iter callee_saved ~f:(fun (reg, slot) ->
      Vec.push
        flat'
        (Flat.Line.Instr
           (Flat.Instr.Mov { dst = Operand.Reg reg; src = Operand.stack_local Q slot }));
      ());
    Vec.freeze flat'
  in
  let flat = Resolve_stack.resolve_function stack_layout flat in
  let prologue = create_prologue stack_layout in
  let epilogue = create_epilogue stack_layout in
  let flat' =
    Vec.create ~size:(List.length prologue + Vec.length flat + List.length epilogue) ()
  in
  Vec.push flat' (Flat.Line.Type (fn.name, "@function"));
  Vec.push flat' (Flat.Line.Global fn.name);
  Vec.push flat' (Flat.Line.Label fn.name);
  List.iter prologue ~f:(fun instr -> Vec.push flat' (Flat.Line.Instr instr));
  Vec.append_into ~into:flat' flat;
  List.iter epilogue ~f:(fun instr -> Vec.push flat' (Flat.Line.Instr instr));
  Vec.freeze flat'
;;

let run (program : _ Program.t) =
  let res_program = Vec.create () in
  Vec.push res_program Flat.Line.SectionText;
  List.iteri program.functions ~f:(fun func_index fn ->
    let flat = run_function ~func_index fn in
    Vec.append_into ~into:res_program flat;
    ());
  Vec.shrink_to_fit res_program;
  Vec.freeze res_program
;;
