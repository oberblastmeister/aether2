open O
open Types
open Utils.Instr_types

let transfer = Dataflow.Liveness.instr_transfer |> Cfg.Dataflow.Instr_transfer.transfer

let add_block_edges interference block live_out =
  let live_out = ref live_out in
  let _ =
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
          Interference.add_edge interference (VReg.to_name def) (VReg.to_name live));
        live_out := transfer instr !live_out))
  in
  ()
;;

let collect_precolored _ = todo ()

let construct_fn fn =
  let _, live_out_facts = Dataflow.Liveness.run fn in
  let interference = Interference.create () in
  (FC.Map.foldi fn.graph.blocks) (fun (label, block) ->
    let live_out = Cfg.Dataflow.Fact_base.find_exn live_out_facts label in
    add_block_edges interference block live_out);
  interference
;;

let alloc_fn fn =
  let interference = construct_fn fn in
  ()
;;
