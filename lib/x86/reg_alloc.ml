open! O
open Ast
open Utils.Instr_types
module Interference = Compiler.Reg_alloc.Interference
module Reg_alloc = Compiler.Reg_alloc

module Config = struct
  module Register = struct
    include Mach_reg
  end

  let config =
    { Reg_alloc.Types.register_order =
        Mach_reg.caller_saved_without_r11 @ Mach_reg.callee_saved_without_stack
    }
  ;;
end

module Ra = Reg_alloc.Make (Config)
module Allocation = Ra.Allocation

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

let alloc_function fn =
  let interference, precolored = construct_fn fn in
  (* each precolored register will be marked as used in the allocation *)
  (* though it will never be in the allocation map *)
  let allocation = Ra.Greedy.run ~precolored ~interference in
  allocation
;;

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
  let graph = Cfg.Graph.map fn.graph ~f:(fun block -> apply_allocation_block cx block) in
  let params = (List.map & Tuple2.map_fst) fn.params ~f:(apply_vreg cx) in
  let stack_params = List.map fn.stack_params ~f:(apply_vreg cx) in
  { fn with graph; params; stack_params }
;;
