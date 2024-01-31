open O
open Types
open Utils.Instr_types

let transfer = Dataflow.Liveness.instr_transfer |> Cfg.Dataflow.Instr_transfer.transfer

let construct_interference_block block live_out =
  let interference = Interference.create () in
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
      F.Iter.product (Instr.defs_fold instr) (FC.Set.iter !live_out)
      |> F.Iter.filter ~f:(fun (def, live) -> is_edge def live)
      |> F.Iter.iter ~f:(fun (def, live) ->
        let def = def |> Reg.vreg_val_exn in
        let live = live |> Reg.vreg_val_exn in
        Interference.add_edge interference (VReg.to_name def) (VReg.to_name live));
      live_out := transfer instr !live_out)
  in
  interference
;;

let construct_fn fn =
  let _, live_out = Dataflow.Liveness.run fn in
;;
