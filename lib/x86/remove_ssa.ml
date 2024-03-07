open O
open Types

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
            { Block.instrs = block.instrs |> map_last ~f:(fun _ -> Jump jump_with_no_args)
            }
          in
          graph
          |> Cfg.Graph.set label block
          |> Cfg.Graph.set j.label (Block.cons (Virt par_mov) to_block)))
  in
  { fn with graph }
;;
