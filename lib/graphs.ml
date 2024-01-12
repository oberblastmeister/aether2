open O

type 'v graph_fold = ('v, 'v) F.Fold.t

module type Node = sig
  type t [@@deriving equal, compare, hash, sexp]

  include Comparator.S with type t := t
end

type 'n node = (module Node with type t = 'n)

type 'n t =
  { node : 'n node
  ; succs : 'n graph_fold
  ; all_nodes : 'n Iter.t
  }

type 'n double =
  { node : 'n node
  ; succs : 'n graph_fold
  ; preds : 'n graph_fold
  ; all_nodes : 'n Iter.t
  }

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

module Dfs = struct
  let visit visitor start_nodes (graph : _ t) =
    let visited = Hash_set.create (node_to_key graph.node) in
    let rec go node =
      Hash_set.add visited node;
      visitor (`Enter node);
      F.Fold.iter graph.succs node ~f:(fun node ->
        if Hash_set.mem visited node then visitor (`Cycle node) else go node);
      visitor (`Exit node)
    in
    List.iter ~f:go start_nodes
  ;;

  let visit_preorder visitor =
    visit (function
      | `Enter node -> visitor node
      | _ -> ())
  ;;

  let visit_postorder visitor =
    visit (function
      | `Exit node -> visitor node
      | _ -> ())
  ;;

  let postorder start_nodes graph =
    let vec = Vec.create () in
    visit_postorder (Vec.push vec) start_nodes graph;
    vec
  ;;

  let reverse_postorder start_nodes graph =
    let vec = postorder start_nodes graph in
    Vec.rev_inplace vec;
    vec
  ;;

  let preorder start_nodes graph =
    let vec = Vec.create () in
    visit_preorder (Vec.push vec) start_nodes graph;
    vec
  ;;
end
