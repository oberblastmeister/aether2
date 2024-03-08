open! Core
open Types
module F = Folds

let t_of_double { all_nodes; succs; _ } = { all_nodes; succs }

let of_map_generic ~iter map =
  { succs = (fun n -> Map.find map n |> Option.value_map ~default:F.Iter.empty ~f:iter)
  ; all_nodes = (fun ~f -> Map.iter_keys map ~f)
  }
;;

let t_of_map_list node = of_map_generic node ~iter:List.iter
let t_of_map_set node = of_map_generic node ~iter:Set.iter

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
  ; preds = (fun n -> Map.find preds n |> Option.value ~default:[] |> List.iter)
  ; all_nodes
  }
;;
