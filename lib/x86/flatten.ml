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
    | Label s -> Label s
    | Stack (End i) -> Int (Imm_int.of_int32 (Stack_layout.end_offset stack i))
    | Stack (Local name) -> Int (Imm_int.of_int32 (Stack_layout.local_offset stack name))
    | Stack (Start i) -> Int (Imm_int.of_int32 (Stack_layout.start_offset stack i))
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
    | Mem addr -> Mem (resolve_address stack addr)
  ;;

  let resolve_instr stack (minstr : _ Flat.Instr.t) : _ Flat.Instr.t =
    let resolve_operand = resolve_operand stack in
    match minstr with
    | Mov { dst; src; s } ->
      Mov { dst = resolve_operand dst; src = resolve_operand src; s }
    | MovZx { dst_size; src_size; dst; src } ->
      MovZx { dst_size; src_size; dst = resolve_operand dst; src = resolve_operand src }
    | MovAbs { dst; imm } -> MovAbs { dst = resolve_operand dst; imm }
    | Lea { dst; src; s } ->
      Lea { dst = resolve_operand dst; src = resolve_operand src; s }
    | Add { dst; src; s } ->
      Add { dst = resolve_operand dst; src = resolve_operand src; s }
    | Imul { dst; src; s } ->
      Imul { dst = resolve_operand dst; src = resolve_operand src; s }
    | Sub { dst; src; s } ->
      Sub { dst = resolve_operand dst; src = resolve_operand src; s }
    | Push { src; s } -> Push { src = resolve_operand src; s }
    | Pop { dst; s } -> Pop { dst = resolve_operand dst; s }
    | Cmp { src1; src2; s } ->
      Cmp { src1 = resolve_operand src1; src2 = resolve_operand src2; s }
    | Test { src1; src2; s } ->
      Test { src1 = resolve_operand src1; src2 = resolve_operand src2; s }
    | Set { dst; cond } -> Set { dst = resolve_operand dst; cond }
    | Call p -> Call p
    | J { cond; src } -> J { cond; src }
    | Jmp { src } -> Jmp { src }
    | Ret -> Ret
    | Div { s; src } -> Div { s; src = resolve_operand src }
    | Idiv { s; src } -> Idiv { s; src = resolve_operand src }
    | Cqto -> Cqto
  ;;

  let resolve_function stack_layout fn =
    Flat.Program.map_instrs fn ~f:(resolve_instr stack_layout)
  ;;
end

let create_prologue stack_layout =
  [ Flat.Instr.Sub
      { s = Q
      ; dst = Reg (MReg.create RSP)
      ; src = Imm (Int (Imm_int.of_int32 (Stack_layout.size stack_layout)))
      }
  ]
;;

let create_epilogue stack_layout =
  [ Flat.Instr.Add
      { s = Q
      ; dst = Reg (MReg.create RSP)
      ; src = Imm (Int (Imm_int.of_int32 (Stack_layout.size stack_layout)))
      }
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
      MReg.create reg, Stack_builder.fresh_stack_slot stack_builder "spill_callee_saved")
    |> F.Iter.to_list
  in
  [%log.global.debug
    (allocation : Reg_alloc.Allocation.t) (callee_saved : (MReg.t * Stack_slot.t) list)];
  let fn = Reg_alloc.apply_allocation_function ~allocation ~stack_builder fn in
  [%log.global.debug "after apply_allocation_function" (fn : AReg.t Function.t)];
  let fn = Remove_ssa.remove_ssa fn in
  let flat = Legalize.legalize_function ~func_index fn in
  [%log.global.debug "legalized" (flat : AReg.t Flat.Program.t)];
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
           (Flat.Instr.Mov
              { s = Q; dst = Operand.stack_local slot; src = Operand.Reg reg }));
      ());
    Vec.append_into ~into:flat' flat;
    (* reload callee saved *)
    List.iter callee_saved ~f:(fun (reg, slot) ->
      Vec.push
        flat'
        (Flat.Line.Instr
           (Flat.Instr.Mov
              { s = Q; dst = Operand.Reg reg; src = Operand.stack_local slot }));
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

let group_data_decls_by_section data_decls =
  List.sort_and_group data_decls ~compare:(fun d1 d2 ->
    Section.compare d1.Data_decl.section d2.section)
;;

let add_data_decl flat (data_decl : Data_decl.t) =
  Vec.push flat (Flat.Line.Align data_decl.align);
  Vec.push flat (Flat.Line.Label data_decl.name);
  match data_decl.data with
  | String s ->
    todo [%here];
    ()
  | Bytes b ->
    String.iter b ~f:(fun c -> Vec.push flat (Flat.Line.Byte c));
    ()
;;

let run (program : _ Program.t) =
  let res_program = Vec.create () in
  Vec.push res_program (Flat.Line.Section ".text");
  List.iteri program.functions ~f:(fun func_index fn ->
    let flat = run_function ~func_index fn in
    Vec.append_into ~into:res_program flat;
    ());
  let data_decls_groups = group_data_decls_by_section program.data_decls in
  List.iter data_decls_groups ~f:(fun data_decl_group ->
    let section = (List.hd_exn data_decl_group).section in
    Vec.push res_program (Flat.Line.Section (Section.to_string section));
    List.iter data_decl_group ~f:(fun data_decl ->
      add_data_decl res_program data_decl;
      ());
    ());
  Vec.shrink_to_fit res_program;
  Vec.freeze res_program
;;
