open! O
open Types
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

let transfer = Dataflow.Liveness.instr_transfer |> Cfg.Dataflow.Instr_transfer.transfer

(* TODO: multiple defs in the same instruction interfere with each other *)
let add_block_edges ~interference ~constraints block live_out =
  let live_out = ref live_out in
  Block.instrs_backward_fold block (fun instr ->
    let defs = Instr.defs_fold instr |> F.Iter.to_list in
    let defs =
      match instr with
      | Instr.Mov { src = Operand.Reg src; _ } -> src :: defs
      | _ -> defs
    in
    let is_edge live =
      (* don't add an edge to what we are currently defining *)
      not @@ List.mem ~equal:[%equal: VReg.t] defs live
    in
    (* make sure we at least add every use in, because the register allocator uses the domain of interference as all nodes *)
    (* TODO: don't add precolored registers in *)
    Instr.regs_fold instr
    |> F.Iter.iter ~f:(fun def -> Interference.add_node interference def.VReg.name);
    (* add interference edges *)
    FC.Set.iter !live_out
    |> F.Iter.filter ~f:(fun live -> is_edge live)
    |> F.Iter.iter ~f:(fun live ->
      Instr.defs_fold instr
      |> F.Iter.iter ~f:(fun def ->
        Interference.add_edge interference def.VReg.name live.VReg.name);
      Instr.mach_reg_defs instr (fun mach_reg ->
        Ra.Constraints.add constraints live.name mach_reg;
        ());
      ());
    ();
    live_out := transfer instr !live_out;
    ());
  ()
;;

(* let collect_precolored fn =
  F.Fold.(
    Function.instrs_forward_fold
    @> Instr.regs_fold
    @> of_fn (fun (vreg : VReg.t) ->
      vreg.precolored |> Option.map ~f:(fun reg -> `name vreg.name, `reg reg))
    @> FC.Option.fold)
    fn
  |> F.Iter.map ~f:(fun (`name name, `reg reg) -> name, reg)
  |> Name.Table.of_iter
;; *)

let construct_fn fn =
  (* let precolored_names =  *)
  let _, live_out_facts = Dataflow.Liveness.run fn in
  let interference = Interference.create () in
  let constraints = Ra.Constraints.create () in
  (Cfg.Graph.to_iteri fn.graph) (fun (label, block) ->
    let live_out = Cfg.Dataflow.Fact_base.find_exn live_out_facts label in
    add_block_edges ~interference ~constraints block live_out);
  interference, constraints
;;

let alloc_fn fn =
  let open Result.Let_syntax in
  let interference, constraints = construct_fn fn in
  (* let precolored = collect_precolored fn in *)
  let%bind allocation =
    Ra.Greedy.run ~precolored:(Name.Table.create ()) ~interference ~constraints
  in
  Ok allocation
;;

let collect_all_vregs fn =
  (Function.instrs_forward_fold @> Instr.regs_fold) fn
  |> F.Iter.map ~f:(fun (reg : VReg.t) -> reg.name, reg)
  |> Name.Table.of_iter
;;

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

module Lower : sig
  val lower_function
    :  Name.Id.t
    -> AReg.t Flat.Program.t
    -> MReg.t Flat.Program.t * (Name.t * int32, read) Vec.t
end = struct
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
end

module Apply : sig
  val apply_allocation_function
    :  allocation:Ra.Allocation.t
    -> VReg.t Function.t
    -> AReg.t Function.t
end = struct
  let apply_allocation_instr ~allocation (instr : VReg.t Instr.t) =
    let spilled = Vec.create () in
    ( spilled |> Vec.freeze
    , Instr.map_regs instr ~f:(fun vreg : AReg.t ->
        match Ra.Allocation.find_exn allocation vreg.name with
        | Spilled ->
          Vec.push spilled (vreg.s, vreg.name);
          Spilled { s = vreg.s; name = vreg.name }
        | InReg reg -> InReg { s = vreg.s; name = Some vreg.name.name; reg }) )
  ;;

  let apply_allocation_block ~allocation (block : VReg.t Block.t) =
    let instrs = Vec.create ~size:(Vec.length block.instrs) () in
    Vec.iter block.instrs ~f:(fun instr ->
      let spilled, instr = apply_allocation_instr ~allocation instr in
      Vec.iter spilled ~f:(fun (s, name) ->
        Vec.push instrs
        @@ Instr.Virt
             (ReserveStackLocal { name; size = Size.to_byte_size s |> Int32.of_int_exn }));
      Vec.push instrs instr);
    { Block.instrs = Vec.freeze instrs }
  ;;

  (* TODO: handle using callee saved registers *)
  let apply_allocation_function ~allocation (fn : VReg.t Function.t) =
    Function.map_blocks fn ~f:(apply_allocation_block ~allocation)
  ;;
end

module Remove_ssa : sig
  val remove_ssa : 'a Function.t -> 'a Function.t
end = struct
  let map_last vec ~f =
    let vec = Vec.copy_exact ~size:(Vec.length vec + 1) vec in
    Vec.set vec (Vec.length vec - 1) (f (Vec.get vec (Vec.length vec - 1)));
    Vec.freeze vec
  ;;

  let remove_ssa (fn : _ Function.t) =
    let graph = fn.graph in
    (* we need this because we are removing the block parameters as we go, so first save them here *)
    let params_of_label =
      Cfg.Graph.to_iteri graph
      |> F.Iter.map ~f:(fun (label, block) ->
        let params = Block.block_args_exn block in
        label, params)
      |> Label.Table.of_iter
    in
    let graph =
      Cfg.Graph.foldi graph ~init:graph ~f:(fun graph (label, block) ->
        let jump = Block.jump_exn block in
        let jump_with_no_args =
          Jump.map_block_calls jump ~f:(fun j -> { j with args = [] })
        in
        let block_calls = Jump.block_calls_fold jump |> F.Iter.to_list in
        let get_data (j : _ Block_call.t) =
          let args = j.args in
          let to_block = Cfg.Graph.find_exn j.label graph in
          let params = Label.Table.find_exn params_of_label j.label in
          let par_mov = List.zip_exn params args |> VInstr.Par_mov in
          to_block, par_mov
        in
        match block_calls with
        | [ j ] ->
          let _to_block, par_mov = get_data j in
          let instrs =
            block.instrs |> fun vec -> Vec.copy_exact ~size:(Vec.length vec + 1) vec
          in
          let _ = Vec.pop_exn instrs in
          Vec.push instrs @@ Instr.Virt par_mov;
          Vec.push instrs @@ Jump jump_with_no_args;
          let instrs = Vec.freeze instrs in
          graph |> Cfg.Graph.set label { Block.instrs }
        | js ->
          List.fold js ~init:graph ~f:(fun graph j ->
            let to_block, par_mov = get_data j in
            let block =
              { Block.instrs =
                  block.instrs |> map_last ~f:(fun _ -> Jump jump_with_no_args)
              }
            in
            graph
            |> Cfg.Graph.set label block
            |> Cfg.Graph.set j.label (Block.cons (Virt par_mov) to_block)))
    in
    { fn with graph }
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
    | Stack (Start _) -> todo ()
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

  let resolve_minstr stack (minstr : _ Flat.Instr.t) =
    let resolve_operand = resolve_operand stack in
    match minstr with
    | Mov { dst; src } ->
      Flat.Instr.Mov { dst = resolve_operand dst; src = resolve_operand src }
    | MovAbs { dst; imm } -> MovAbs { dst = resolve_operand dst; imm }
    | Lea { dst; src } -> Lea { dst = resolve_operand dst; src = resolve_operand src }
    | Add { dst; src } -> Add { dst = resolve_operand dst; src = resolve_operand src }
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
  ;;

  let resolve_function stack_layout fn =
    Flat.Program.map_instrs fn ~f:(resolve_minstr stack_layout)
  ;;
end

let run_function fn =
  (* print_s [%message (fn : VReg.t Function.t)]; *)
  let allocation = alloc_fn fn |> Or_error.ok_exn in
  (* print_s [%message (allocation : Ra.Allocation.t)]; *)
  let fn = Apply.apply_allocation_function ~allocation fn in
  (* print_s [%message "apply_allocation" (fn : AReg.t Function.t)]; *)
  (* something is wrong with the remove_ssa function, we need to add entry and exit to block *)
  let fn = Remove_ssa.remove_ssa fn in
  (* print_s [%message "remove_ssa" (fn : AReg.t Function.t)]; *)
  let flat = Legalize.legalize_function fn in
  (* print_s [%message "legalized" (fn : AReg.t Function.t)]; *)
  let flat, stack_locals = Lower.lower_function fn.unique_name flat in
  let stack_layout =
    Stack_layout.create ~end_size:fn.stack_end_size ~locals:stack_locals
  in
  (* print_s [%message "lowered" (fn : MReg.t Function.t)]; *)
  let flat = Resolve_stack.resolve_function stack_layout flat in
  (* print_s [%message "resolved" (fn : MReg.t Function.t)]; *)
  flat
;;

let run program =
  let res_program = Vec.create () in
  Program.iter_functions program ~f:(fun fn ->
    let flat = run_function fn in
    Vec.append_into ~into:res_program flat;
    ());
  Vec.shrink_to_fit res_program;
  Vec.freeze res_program
;;
