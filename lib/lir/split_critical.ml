open! O
open Types

let split pred (fn : Vir.Function.t) =
  let preds =
    Data.Constructors.Hashtbl_ext.project
    @@ Data.Graph.get_pred_map (Data.Constructors.hashtbl (module Label))
    @@ Graph.to_graph fn.graph
  in
  Function.with_mut fn (fun mut_fn ->
    Cfg.Graph.iteri mut_fn.graph (fun (label, block) ->
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
                Hashtbl.find preds block_call.label
                |> Option.value ~default:[]
                |> List.length
              in
              if num_preds_target > 1
              then (
                let new_label = Mut_function.fresh_label mut_fn block_call.label.name in
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
