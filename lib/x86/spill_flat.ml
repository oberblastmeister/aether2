open O
open Ast
open Flat

type context =
  { instrs : (MReg.t Flat.Line.t, read_write) Vec.t (* ; stack_layout : Stack_layout.t *)
  ; stack_builder : Stack_builder.t
  }

module Cx = struct
  let create stack_builder = { instrs = Vec.create (); stack_builder }
  let add cx = Vec.push cx.instrs
  let add cx = Vec.push cx.instrs
  let add_instr cx instr = Vec.push cx.instrs (Instr instr)

  let spill_reg cx reg =
    let spill_slot = Stack_builder.stack_slot_of_mach_reg cx.stack_builder reg in
    add_instr cx @@ Flat.Instr.mov_to_stack_from_reg Q spill_slot reg
  ;;

  let reload_reg cx reg =
    let spill_slot = Stack_builder.stack_slot_of_mach_reg cx.stack_builder reg in
    add_instr cx @@ Flat.Instr.mov_to_reg_from_stack Q reg spill_slot
  ;;
end

let lower_instr cx instr =
  let module Set = Mach_reg_set in
  let module Enum_set = Data.Enum_set in
  let unused_registers =
    let set = Set.create () in
    Flat.Instr.iter_regs instr
    |> F.Iter.filter_map ~f:AReg.reg_val
    |> F.Iter.iter ~f:(fun reg -> Set.add set reg);
    Enum_set.negate set;
    Set.remove set R11;
    Set.iter set |> F.Iter.to_list
  in
  let spilled =
    Flat.Instr.iter_regs instr
    |> F.Iter.filter_map ~f:(fun areg ->
      match areg with
      | AReg.Spilled { name; _ } -> Some name
      | _ -> None)
    |> F.Iter.to_list
  in
  if not @@ List.is_empty spilled
  then (
    [%log.global.debug
      "need to spill"
        (instr : AReg.t Instr.t)
        (unused_registers : Mach_reg.t list)
        (spilled : Stack_slot.t list)];
    ());
  assert (List.length spilled <= 9);
  let stack_slot_and_victim, remaining =
    List.zip_with_remainder spilled unused_registers
  in
  (match remaining with
   | Some (Second _) -> ()
   | Some (First _) | None ->
     raise_s
       [%message
         "impossible, list of registers must be greater than number of spilled because \
          we have only 9 maximum spills"]);
  let victims = List.map stack_slot_and_victim ~f:snd in
  (* spill victims *)
  List.iter victims ~f:(Cx.spill_reg cx);
  (* move spilled uses to the victims *)
  Flat.Instr.iter_uses instr
  |> F.Iter.filter_map ~f:AReg.spilled_val
  |> F.Iter.iter ~f:(fun (`s s, `name spilled) ->
    let reg = List.Assoc.find_exn ~equal:Stack_slot.equal stack_slot_and_victim spilled in
    Cx.add_instr cx @@ Flat.Instr.mov_to_reg_from_stack s reg spilled;
    ());
  (* use the victims instead of the stack slots *)
  let instr_with_victims =
    Flat.Instr.map_regs instr ~f:(fun areg ->
      match areg with
      | Spilled { s; name } ->
        MReg.create
          ~name:name.name
          s
          (List.Assoc.find_exn ~equal:Stack_slot.equal stack_slot_and_victim name)
      | InReg { s; name; reg } -> MReg.create ?name s reg)
  in
  Cx.add_instr cx instr_with_victims;
  (* move defined victim registers to the stack slot *)
  Flat.Instr.iter_defs instr (fun def ->
    match def with
    | Spilled { s; name } ->
      let victim =
        List.Assoc.find_exn ~equal:Stack_slot.equal stack_slot_and_victim name
      in
      Cx.add_instr cx @@ Flat.Instr.mov_to_stack_from_reg s name victim;
      ()
    | _ -> ());
  (* reload victims *)
  List.iter victims ~f:(Cx.reload_reg cx)
;;

let lower_block_call { Block_call.label; _ } = { Block_call.label; args = [] }

let lower_jump = function
  | Jump.Jump j -> Jump.Jump (lower_block_call j)
  | Jump.CondJump { cond; j1; j2 } ->
    Jump.CondJump { cond; j1 = lower_block_call j1; j2 = lower_block_call j2 }
  | Jump.Ret r -> Jump.Ret r
;;

let lower_instr cx instr = lower_instr cx instr

let lower_function stack_builder (fn : _ Flat.Program.t) =
  let cx = Cx.create stack_builder in
  Vec.iter fn ~f:(function
    | Flat.Line.Instr instr -> lower_instr cx instr
    | Label l -> Cx.add cx @@ Flat.Line.Label l
    | Comment s -> Cx.add cx @@ Comment s);
  Vec.shrink_to_fit cx.instrs;
  let instrs = Vec.freeze cx.instrs in
  instrs
;;
