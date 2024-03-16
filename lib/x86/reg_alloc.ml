open! O
open Ast
open Utils.Instr_types
module Interference = Compiler.Reg_alloc.Interference
module Reg_alloc = Compiler.Reg_alloc

module Ra = Reg_alloc.Make (struct
    module Register = struct
      include Mach_reg
    end

    let config =
      { Reg_alloc.Types.register_order =
          Mach_reg.caller_saved_without_r11 @ Mach_reg.callee_saved_without_stack
      }
    ;;
  end)

module Precolored : sig
  type t [@@deriving sexp_of]

  val create : Name.Id.t -> t
  val get_name : t -> Mach_reg.t -> Name.t

  (* after this is called none of the other functions should be called *)
  val get_precolored : t -> (Name.t * Mach_reg.t) list
end = struct
  type t =
    { name_of_mach_reg : (Mach_reg.t, Name.t) Hashtbl.t
    ; mach_reg_of_name : (Name.t, Mach_reg.t) Hashtbl.t
    ; mutable unique_name : Name.Id.t
    }
  [@@deriving sexp_of]

  let create unique_name =
    { name_of_mach_reg = Hashtbl.create (module Mach_reg)
    ; mach_reg_of_name = Hashtbl.create (module Name)
    ; unique_name
    }
  ;;

  let get_name t mach_reg =
    match Hashtbl.find t.name_of_mach_reg mach_reg with
    | Some name -> name
    | None ->
      let name =
        Name.create (Sexp.to_string_mach (Mach_reg.sexp_of_t mach_reg)) t.unique_name
      in
      t.unique_name <- Name.Id.next t.unique_name;
      Hashtbl.add_exn t.name_of_mach_reg ~key:mach_reg ~data:name;
      Hashtbl.add_exn t.mach_reg_of_name ~key:name ~data:mach_reg;
      name
  ;;

  let get_precolored t = t.mach_reg_of_name |> Hashtbl.to_alist
end

let transfer = Dataflow.Liveness.instr_transfer |> Cfg.Dataflow.Instr_transfer.transfer

let iter_pairs xs ~f =
  let rec go xs =
    match xs with
    | [] -> ()
    | x :: xs ->
      List.iter xs ~f:(fun y -> f (x, y));
      go xs
  in
  go xs
;;

(* TODO: multiple defs in the same instruction interfere with each other *)
let add_block_edges ~interference ~precolored block live_out =
  let live_out = ref live_out in
  Block.iter_instrs_backward block ~f:(fun instr ->
    let out_list = Set.to_list !live_out in
    let uses = Instr.iter_uses instr |> F.Iter.to_list in
    [%log.global.debug
      (out_list : VReg.t list) (instr : VReg.t Instr.t) (uses : VReg.t list)];
    let defs = Instr.iter_defs instr |> F.Iter.to_list in
    (* ensure that multiple defs interfere with each other *)
    iter_pairs defs ~f:(fun (def1, def2) ->
      Interference.add_edge interference def1.VReg.name def2.name);
    let is_edge =
      let defs =
        (* dst and src in movs are in the same equivalence class *)
        match instr with
        | Instr.Mov { src = Operand.Reg src; _ } -> src :: defs
        | _ -> defs
      in
      fun live ->
        (* don't add an edge to what we are currently defining *)
        not @@ List.mem ~equal:[%equal: VReg.t] defs live
    in
    (* make sure we at least add every use/def in, because the register allocator uses the domain of interference as all nodes *)
    Instr.iter_regs instr
    |> F.Iter.iter ~f:(fun def -> Interference.add_node interference def.VReg.name);
    (* don't add the mach regs, we want to add them lazily *)
    (* this way we don't get bogus uses for precolored mach regs*)
    (* Instr.mach_reg_defs instr (fun mach_reg ->
       let _ = Precolored.get_name precolored mach_reg in
       ()); *)

    (* add interference edges *)
    Set.iter !live_out
    |> F.Iter.filter ~f:(fun live -> is_edge live)
    |> F.Iter.iter ~f:(fun live ->
      List.iter defs
      |> F.Iter.iter ~f:(fun def ->
        Interference.add_edge interference def.VReg.name live.VReg.name);
      Instr.mach_reg_defs instr (fun mach_reg ->
        let name = Precolored.get_name precolored mach_reg in
        Interference.add_edge interference name live.name;
        ());
      ());
    ();
    live_out := transfer instr !live_out;
    ());
  ()
;;

(* we need this for precolored node names *)
let%expect_test _ =
  print_endline (Sexp.to_string_mach (Mach_reg.sexp_of_t RAX));
  [%expect {| RAX |}]
;;

let construct_fn (fn : _ Function.t) =
  let _, live_out_facts = Dataflow.Liveness.run fn in
  let interference = Interference.create () in
  (* function parameters must interfere with each other *)
  Function.all_params fn
  |> iter_pairs ~f:(fun (param1, param2) ->
    Interference.add_edge interference param1.VReg.name param2.VReg.name);
  let precolored = Precolored.create fn.unique_name in
  Cfg.Graph.iteri fn.graph ~f:(fun (label, block) ->
    let live_out = Cfg.Dataflow.Fact_base.find_exn live_out_facts label in
    add_block_edges ~interference ~precolored block live_out;
    ());
  [%log.global.debug
    (fn.name : string) (interference : Interference.t) (precolored : Precolored.t)];
  interference, Precolored.get_precolored precolored
;;

let alloc_fn fn =
  let interference, precolored = construct_fn fn in
  (* each precolored register will be marked as used in the allocation *)
  (* though it will never be in the allocation map *)
  let allocation = Ra.Greedy.run ~precolored ~interference in
  allocation
;;

module Apply : sig
  val apply_allocation_function
    :  allocation:Ra.Allocation.t
    -> stack_builder:Stack_builder.t
    -> VReg.t Function.t
    -> AReg.t Function.t
end = struct
  type cx =
    { allocation : Ra.Allocation.t
    ; stack_builder : Stack_builder.t
    }

  let apply_vreg cx (vreg : VReg.t) : AReg.t =
    match Ra.Allocation.find_exn cx.allocation vreg.name with
    | Spilled ->
      let stack_slot = Stack_builder.stack_slot_of_name cx.stack_builder vreg.name in
      Spilled { s = vreg.s; name = stack_slot }
    | InReg reg -> InReg { s = vreg.s; name = Some vreg.name.name; reg }
  ;;

  let apply_allocation_instr cx (instr : VReg.t Instr.t) =
    Instr.map_regs instr ~f:(apply_vreg cx)
  ;;

  let apply_allocation_block cx (block : VReg.t Block.t) =
    let instrs =
      Vec.map_copy block.instrs ~f:(fun instr -> apply_allocation_instr cx instr)
    in
    { Block.instrs = Vec.freeze instrs }
  ;;

  (* TODO: handle using callee saved registers *)
  let apply_allocation_function ~allocation ~stack_builder (fn : VReg.t Function.t) =
    let cx = { allocation; stack_builder } in
    let graph =
      Graph.map_blocks fn.graph ~f:(fun block -> apply_allocation_block cx block)
    in
    let params = (List.map & Tuple2.map_fst) fn.params ~f:(apply_vreg cx) in
    let stack_params = List.map fn.stack_params ~f:(apply_vreg cx) in
    { fn with graph; params; stack_params }
  ;;
end

module Resolve_stack : sig
  val resolve_function : Stack_layout.t -> MReg.t Flat.Program.t -> MReg.t Flat.Program.t
end = struct
  let resolve_imm stack imm =
    match imm with
    | Imm.Int i -> Imm.Int i
    | Stack (End i) -> Int (Stack_layout.end_offset stack i)
    | Stack (Local name) -> Int (Stack_layout.local_offset stack name)
    | Stack (Start _) -> todo [%here]
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

  let resolve_minstr stack (minstr : _ Flat.Instr.t) : _ Flat.Instr.t =
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
    Flat.Program.map_instrs fn ~f:(resolve_minstr stack_layout)
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
  let allocation = alloc_fn fn in
  let callee_saved =
    Ra.Allocation.used_registers allocation
    |> F.Iter.filter ~f:(fun reg ->
      List.mem ~equal:Mach_reg.equal Mach_reg.callee_saved reg)
    |> F.Iter.map ~f:(fun reg ->
      MReg.create Q reg, Stack_builder.fresh_stack_slot stack_builder "spill_callee_saved")
    |> F.Iter.to_list
  in
  [%log.global.debug
    (allocation : Ra.Allocation.t) (callee_saved : (MReg.t * Stack_slot.t) list)];
  let fn = Apply.apply_allocation_function ~allocation ~stack_builder fn in
  let fn = Remove_ssa.remove_ssa fn in
  let flat = Legalize.legalize_function ~func_index fn in
  let flat = Spill_flat.lower_function stack_builder flat in
  let stack_layout = Stack_layout.create (Stack_builder.get_stack_instrs stack_builder) in
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
