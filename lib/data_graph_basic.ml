open! O
open Data_graph_types

let node_to_key (type v) (module Node : Node with type t = v) =
  (module Node : Hashtbl.Key_plain with type t = v)
;;

let t_of_double { all_nodes; succs; _ } = { all_nodes; succs }

let of_map_generic ~iter map =
  { succs = (fun n -> Map.find map n |> Option.value_map ~default:Iter.empty ~f:iter)
  ; all_nodes = (fun k -> Map.iter_keys map ~f:k)
  }
;;

let t_of_map_list node = of_map_generic node ~iter:F.Iter.of_list
let t_of_map_set node = of_map_generic node ~iter:FC.Set.iter

let get_pred_map (type k h) (map : (k, h) Constructors.map) ({ succs; all_nodes } : _ t) =
  let module Map = (val map) in
  let preds = Map.create () in
  F.Iter.iter all_nodes ~f:(fun n ->
    succs n
    |> F.Iter.iter ~f:(fun n' ->
      Map.change preds n' ~f:(fun o ->
        Option.value_map ~default:[ n ] ~f:(List.cons n) o |> Option.some)));
  preds
;;

let double_of_t
  (type k)
  (map : k Constructors.some_map)
  (({ succs; all_nodes } : _ t) as t)
  =
  let module Map = (val map) in
  let preds = get_pred_map (module Map) t in
  { succs
  ; preds = (fun n -> Map.find preds n |> Option.value ~default:[] |> F.Iter.of_list)
  ; all_nodes
  }
;;
