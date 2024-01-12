open O
open Data_graph_types

let node_to_key (type v) (module Node : Node with type t = v) =
  (module Node : Hashtbl.Key_plain with type t = v)
;;

let t_of_double { all_nodes; node; succs; _ } = { all_nodes; node; succs }

let get_pred_map ({ node; succs; all_nodes } : _ t) =
  let preds = Hashtbl.create (node_to_key node) in
  F.Iter.iter all_nodes ~f:(fun n ->
    succs n
    |> F.Iter.iter ~f:(fun n' ->
      Hashtbl.update preds n' ~f:(Option.value_map ~default:[ n ] ~f:(List.cons n))));
  preds
;;

let double_of_t (({ node; succs; all_nodes } : _ t) as t) =
  let preds = get_pred_map t in
  { node
  ; succs
  ; preds = (fun n -> Hashtbl.find_multi preds n |> F.Iter.of_list)
  ; all_nodes
  }
;;
