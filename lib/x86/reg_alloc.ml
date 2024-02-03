open O
open Types
open Utils.Instr_types
module Interference = Compiler.Interference

module Reg_alloc = Compiler.Reg_alloc.Make (struct
    module Register = MachReg

    module RegisterSet = struct
      type t = Register.t Hash_set.t [@@deriving sexp_of]
      type value = Register.t

      let create () = Hash_set.create (module Register)
      let mem = Hash_set.mem
      let add = Hash_set.add
    end
  end)

module NameMap = Entity.Map.Make (Name)

let transfer = Dataflow.Liveness.instr_transfer |> Cfg.Dataflow.Instr_transfer.transfer

let add_block_edges interference block live_out =
  let live_out = ref live_out in
  Block.instrs_backward_fold block (fun instr ->
    let without =
      match instr with
      | Instr.Mov { src = Operand.Reg src; _ } -> [ src ]
      | _ -> []
    in
    let is_edge def live =
      (* don't add an edge to itself *)
      (not ([%equal: Reg.t] def live))
      (* don't add an edge to something that is a register mov *)
      (* we want these to be allocated to the same register, so we can remove the redundant mov *)
      && not (List.mem without live ~equal:[%equal: Reg.t])
    in
    (* make sure we at least add the defs in, because the register allocator uses the domain of interference as all nodes *)
    Instr.defs_fold instr
    |> F.Iter.iter ~f:(fun def ->
      Interference.add_node interference @@ VReg.to_name @@ Reg.vreg_val_exn def);
    (* add interference edges *)
    F.Iter.(
      product (Instr.defs_fold instr) (FC.Set.iter !live_out)
      |> filter ~f:(fun (def, live) -> is_edge def live)
      |> iter ~f:(fun (def, live) ->
        let def = def |> Reg.vreg_val_exn in
        let live = live |> Reg.vreg_val_exn in
        Interference.add_edge interference (VReg.to_name def) (VReg.to_name live);
        ()));
    live_out := transfer instr !live_out;
    ());
  ()
;;

let collect_precolored fn =
  F.Fold.(
    Function.instrs_forward_fold
    @> Instr.regs_fold
    @> of_fn Reg.vreg_val_exn
    @> of_fn VReg.precolored_val
    @> FC.Option.fold)
    fn
  |> F.Iter.map ~f:(fun (`name name, `reg reg) -> name, reg)
  |> NameMap.of_iter
;;

let construct_fn fn =
  let _, live_out_facts = Dataflow.Liveness.run fn in
  let interference = Interference.create () in
  (FC.Map.foldi fn.graph.blocks) (fun (label, block) ->
    let live_out = Cfg.Dataflow.Fact_base.find_exn live_out_facts label in
    add_block_edges interference block live_out);
  interference
;;

let register_order =
  MachReg.
    [ (* callee saved *)
      RAX
    ; RDI
    ; RSI
    ; RDX
    ; RCX
    ; R8
    ; R9
    ; R10
    ; (* R11 used for scratch register *)
      (* caller saved *)
      RBX
    ; R12
    ; R13
    ; R14
    ; R15
    ]
;;

let alloc_fn fn =
  let open Result.Let_syntax in
  let interference = construct_fn fn in
  let precolored = collect_precolored fn in
  let%bind allocation = Reg_alloc.run ~precolored ~interference ~register_order in
  Ok allocation
;;

let apply_allocation_instr ~allocation instr =
  match instr with
  | _ -> ()
;;

let apply_allocation_block ~allocation (block : Block.t) =
  let instrs = Vec.create () in
  (Block.instrs_forward_fold block) (fun instr -> ());
  ()
;;
