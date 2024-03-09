open O
open Types
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

let lower_minstr cx minstr =
  let module Set = Mach_reg_set in
  let module Enum_set = Data.Enum_set in
  let unused_registers =
    let set = Set.create () in
    Flat.Instr.iter_regs minstr
    |> F.Iter.filter_map ~f:AReg.reg_val
    |> F.Iter.iter ~f:(fun reg -> Set.add set reg);
    Enum_set.negate set;
    Set.remove set R11;
    Set.iter set |> F.Iter.to_list
  in
  let spilled =
    Flat.Instr.iter_regs minstr
    |> F.Iter.filter_map ~f:(fun areg ->
      match areg with
      | AReg.Spilled { name; _ } -> Some name
      | _ -> None)
    |> F.Iter.to_list
  in
  assert (List.length spilled <= 9);
  let name_and_victim, remaining = List.zip_with_remainder spilled unused_registers in
  (match remaining with
   | Some (Second _) -> ()
   | Some (First _) | None ->
     raise_s
       [%message
         "impossible, list of registers must be greater than number of spilled because \
          we have only 9 maximum spills"]);
  let victims = List.map name_and_victim ~f:snd in
  (* spill victims *)
  List.iter victims ~f:(Cx.spill_reg cx);
  let victim_of_spilled =
    List.iter name_and_victim |> Hashtbl.of_iter (module Stack_slot)
  in
  (* move spilled uses to the victims *)
  Flat.Instr.iter_uses minstr
  |> F.Iter.filter_map ~f:AReg.spilled_val
  |> F.Iter.iter ~f:(fun (`s s, `name spilled) ->
    let reg = Hashtbl.find_exn victim_of_spilled spilled in
    Cx.add_instr cx @@ Flat.Instr.mov_to_reg_from_stack s reg spilled;
    ());
  (* use the victims instead of the stack slots *)
  let minstr_with_victims =
    Flat.Instr.map_regs minstr ~f:(fun areg ->
      match areg with
      | Spilled { s; name } ->
        MReg.create ~name:name.name s (Hashtbl.find_exn victim_of_spilled name)
      | InReg { s; name; reg } -> MReg.create ?name s reg)
  in
  Cx.add_instr cx minstr_with_victims;
  (* move defined victim registers to the stack slot *)
  Flat.Instr.iter_defs minstr (fun def ->
    match def with
    | Spilled { s; name } ->
      let victim = Hashtbl.find_exn victim_of_spilled name in
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

(* | Jump.Ret _ -> raise_s [%message "jump must be legalized"] *)

let lower_instr cx instr = lower_minstr cx instr

(* let lower_block cx (block : _ Block.t) =
    (Block.iter_instrs_forward block) (lower_instr cx);
    let instrs = Vec.copy_exact cx.instrs in
    Vec.clear cx.instrs;
    { Block.instrs }
  ;; *)

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
(* let fn = Function.map_blocks fn ~f:(lower_block cx) in
    { fn with unique_name = cx.unique_name } *)
