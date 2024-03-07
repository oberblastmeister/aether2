open O
open Types
open Flat

type context =
  { instrs : (MReg.t Flat.Line.t, read_write) Vec.t (* ; stack_layout : Stack_layout.t *)
  ; mutable unique_name : Name.Id.t
  ; spill_slots : (Mach_reg.t, Name.t) Hashtbl.t
  ; stack_locals : (Name.t * int32, read_write) Vec.t
  }

module Cx = struct
  let create unique_name =
    { instrs = Vec.create ()
    ; unique_name
    ; spill_slots = Hashtbl.create (module Mach_reg)
    ; stack_locals = Vec.create ()
    }
  ;;

  let add cx = Vec.push cx.instrs

  let fresh_name cx s =
    let name = Name.create s cx.unique_name in
    cx.unique_name <- Name.Id.next cx.unique_name;
    name
  ;;

  let get_spill_slot cx reg =
    match Hashtbl.find cx.spill_slots reg with
    | Some name -> name
    | None ->
      let name = fresh_name cx "spill_mach_reg" in
      Hashtbl.add_exn cx.spill_slots ~key:reg ~data:name;
      Vec.push cx.stack_locals (name, 0l);
      name
  ;;

  let add cx = Vec.push cx.instrs
  let add_instr cx instr = Vec.push cx.instrs (Instr instr)

  let spill_reg cx reg =
    let spill_slot = get_spill_slot cx reg in
    add_instr cx @@ Flat.Instr.mov_to_stack_from_reg Q spill_slot reg
  ;;

  let reload_reg cx reg =
    let spill_slot = get_spill_slot cx reg in
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
    (fun f -> Set.iter set ~f) |> Iter.to_list
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
    F.Iter.of_list name_and_victim |> FC.Hashtbl.of_iter (module Name)
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
    (Block.instrs_forward_fold block) (lower_instr cx);
    let instrs = Vec.copy_exact cx.instrs in
    Vec.clear cx.instrs;
    { Block.instrs }
  ;; *)

let lower_function unique_name (fn : _ Flat.Program.t) =
  let cx = Cx.create unique_name in
  Vec.iter fn ~f:(function
    | Flat.Line.Instr instr -> lower_instr cx instr
    | Label l -> Cx.add cx @@ Flat.Line.Label l
    | Comment s -> Cx.add cx @@ Comment s);
  Vec.shrink_to_fit cx.instrs;
  Vec.shrink_to_fit cx.stack_locals;
  let instrs = Vec.freeze cx.instrs in
  let stack_locals = Vec.freeze cx.stack_locals in
  instrs, stack_locals
;;
(* let fn = Function.map_blocks fn ~f:(lower_block cx) in
    { fn with unique_name = cx.unique_name } *)
