open! O
open Ast

(* A an edge from A to B is critical if A has multiple successors and B has multiple predecessors
   To split all critical edges, find every block with multiple jumps.
   Then make sure that each block that is jumped to has only one predecessor (block that jumps to it).
   If not, then split the edge.
   This is important because if an edge from A to B is critical, then we cannot eliminate phi functions.
   This is because phi functions translate to moves.
   If A has only one successor, then we can put the move right at the end of A.
   If A has multiple successors, then we can put the moves right at the start of each B that is a successor,
   ONLY IF each successor B has only one predecessor.
*)
let split_fn_with ~f (fn : Vir.Function.t) =
  let preds =
    Data.Constructors.Hashtbl_ext.project
    @@ Data.Graph.get_pred_map (Data.Constructors.hashtbl (module Label))
    @@ Graph.to_graph fn.graph
  in
  Function.with_mut fn (fun mut_fn ->
    Cfg.Graph.iteri mut_fn.graph ~f:(fun (label, block) ->
      if f block
      then (
        let block_calls_count =
          F.Fold.reduce Control_instr.iter_block_calls F.Reduce.count block.exit
        in
        (* are there multiple successors? *)
        if block_calls_count > 1
        then (
          let block =
            (Block.map_exit & Control_instr.map_block_calls) block ~f:(fun block_call ->
              let num_preds_target =
                Hashtbl.find preds block_call.label
                |> Option.value ~default:[]
                |> List.length
              in
              (* are there multiple predecessors? *)
              if num_preds_target > 1
              then (
                (* this edge is critical, let's split it *)
                (* this new block only has one predecessor, so our edge isn't critical anymore *)
                let new_label = Mut_function.fresh_label mut_fn block_call.label.name in
                let new_block =
                  (* the new block jumps to the original block call *)
                  { entry = []; Block.body = []; exit = Control_instr.Jump block_call }
                in
                Mut_function.add_block_exn mut_fn new_label new_block;
                { Block_call.label = new_label; args = [] })
              else block_call)
          in
          Mut_function.set_block mut_fn label block))
      else ()))
;;

let split_with ~f prog =
  (Program.map_functions & List.map) prog ~f:(fun fn -> split_fn_with ~f fn)
;;

let split = split_with ~f:(fun _ -> true)
