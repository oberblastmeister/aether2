open O
include Lir_instr

let split pred (fn : Vir.Function.t) =
  let predecessors = Graph.predecessors_of_label fn.graph in
  Function.with_mut fn (fun mut_fn ->
    Map.iteri mut_fn.graph.blocks ~f:(fun ~key:label ~data:block ->
      if pred block
      then (
        let block_calls_count =
          F.Fold.reduce Control_instr.block_calls_fold F.Reduce.count block.exit
        in
        if block_calls_count > 1
        then (
          let block =
            (Block.map_exit & Control_instr.map_block_calls) block ~f:(fun block_call ->
              let num_preds_target =
                Map.find predecessors block_call.label
                |> Option.value ~default:[]
                |> List.length
              in
              if num_preds_target > 1
              then (
                let new_label =
                  Mut_function.fresh_label mut_fn (Label.to_string block_call.label)
                in
                let new_block =
                  { entry = []; Block.body = []; exit = Control_instr.Jump block_call }
                in
                Mut_function.add_block_exn mut_fn new_label new_block;
                { Block_call.label = new_label; args = [] })
              else block_call)
          in
          Mut_function.set_block mut_fn label block))
      else ()))
;;
