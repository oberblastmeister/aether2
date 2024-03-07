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

module Precolored : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
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

  let create () =
    { name_of_mach_reg = Hashtbl.create (module Mach_reg)
    ; mach_reg_of_name = Hashtbl.create (module Name)
    ; unique_name = Name.Id.of_int 0
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

(* TODO: multiple defs in the same instruction interfere with each other *)
let add_block_edges ~interference ~precolored block live_out =
  let live_out = ref live_out in
  Block.instrs_backward_fold block ~f:(fun instr ->
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
    (* make sure we at least add every use/def in, because the register allocator uses the domain of interference as all nodes *)
    Instr.regs_fold instr
    |> F.Iter.iter ~f:(fun def -> Interference.add_node interference def.VReg.name);
    Instr.mach_reg_defs instr (fun mach_reg ->
      Interference.add_node interference (Precolored.get_name precolored mach_reg);
      ());
    (* add interference edges *)
    Set.iter !live_out
    |> F.Iter.filter ~f:(fun live -> is_edge live)
    |> F.Iter.iter ~f:(fun live ->
      Instr.defs_fold instr
      |> F.Iter.iter ~f:(fun def ->
        Interference.add_edge interference def.VReg.name live.VReg.name);
      Instr.mach_reg_defs instr (fun mach_reg ->
        Interference.add_edge
          interference
          (Precolored.get_name precolored mach_reg)
          live.name;
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
  let precolored = Precolored.create () in
  Cfg.Graph.iteri fn.graph ~f:(fun (label, block) ->
    let live_out = Cfg.Dataflow.Fact_base.find_exn live_out_facts label in
    add_block_edges ~interference ~precolored block live_out;
    ());
  interference, Precolored.get_precolored precolored
;;

let alloc_fn fn =
  let open Result.Let_syntax in
  let interference, precolored = construct_fn fn in
  let allocation = Ra.Greedy.run ~precolored ~interference in
  allocation
;;

let collect_all_vregs fn =
  (Function.instrs_forward_fold @> Instr.regs_fold) fn
  |> F.Iter.map ~f:(fun (reg : VReg.t) -> reg.name, reg)
  |> Name.Table.of_iter
;;

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
  let allocation = alloc_fn fn in
  (* print_s [%message (allocation : Ra.Allocation.t)]; *)
  let fn = Apply.apply_allocation_function ~allocation fn in
  (* print_s [%message "apply_allocation" (fn : AReg.t Function.t)]; *)
  (* something is wrong with the remove_ssa function, we need to add entry and exit to block *)
  let fn = Remove_ssa.remove_ssa fn in
  (* print_s [%message "remove_ssa" (fn : AReg.t Function.t)]; *)
  let flat = Legalize.legalize_function fn in
  (* print_s [%message "legalized" (fn : AReg.t Function.t)]; *)
  let flat, stack_locals = Spill_flat.lower_function fn.unique_name flat in
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
