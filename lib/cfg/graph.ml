open! O
open Utils.Instr_types
include Graph_intf

type 'b t =
  { entry : Label.t
  ; blocks : 'b Label.Map.t
  ; exit : Label.t
  }
[@@deriving sexp, fields]

module type Gen_S = Gen_S with type 'b t := 'b t

(* TODO: change this to return option *)
let of_alist ~entry ~exit blocks = { entry; blocks = Label.Map.of_alist_exn blocks; exit }
let to_alist { blocks; _ } = Map.to_alist blocks
let map_blocks graph ~f = { graph with blocks = f graph.blocks }
let get_block_exn graph label = Map.find_exn graph.blocks label
let find label graph = Map.find graph.blocks label
let find_exn label graph = Map.find_exn graph.blocks label

let set label block graph =
  map_blocks graph ~f:(fun blocks -> Map.set ~key:label ~data:block blocks)
;;

let iter_labels graph ~f = Map.iter_keys graph.blocks ~f

let mapi graph ~f =
  map_blocks graph ~f:(Map.mapi ~f:(fun ~key:label ~data:block -> f (label, block)))
;;

let map graph ~f = map_blocks graph ~f:(Map.map ~f)

let foldi graph ~init ~f =
  Map.fold graph.blocks ~init ~f:(fun ~key:label ~data:block acc -> f acc (label, block))
;;

let fold graph ~init ~f =
  Map.fold graph.blocks ~init ~f:(fun ~key:_ ~data:block acc -> f acc block)
;;

let iteri graph ~f = foldi graph ~init:() ~f:(fun () x -> f x)
let iter graph ~f = fold graph ~init:() ~f:(fun () x -> f x)
let add_exn graph label block = map_blocks graph ~f:(Map.add_exn ~key:label ~data:block)
let set_blocks_alist blocks graph = { graph with blocks = Label.Map.of_alist_exn blocks }

let validate graph =
  let _ = Map.find_exn graph.blocks graph.entry in
  let _ = Map.find_exn graph.blocks graph.exit in
  ()
;;

let to_graph ~jumps graph =
  { Data.Graph.succs = (fun label -> jumps (Map.find_exn graph.blocks label))
  ; all_nodes = (fun ~f -> Map.iter_keys graph.blocks ~f)
  }
;;

let to_double_graph ~jumps graph =
  to_graph ~jumps graph
  |> Data.Graph.double_of_t (Data.Constructors.some_hashtbl (module Label))
;;

let predecessors_of_label ~jumps (graph : 'b t) =
  graph.blocks
  |> Map.to_alist
  |> List.bind ~f:(fun (jumped_from, block) ->
    jumps block |> F.Iter.to_list |> List.map ~f:(fun jumped_to -> jumped_to, jumped_from))
  |> Label.Map.of_alist_multi
;;

let map_simple_order (graph : 'b t) ~(f : Label.t * 'b -> 'b) =
  let start_block =
    Map.find_exn graph.blocks graph.entry |> fun block -> f (graph.entry, block)
  in
  let blocks =
    Map.mapi
      ~f:(fun ~key:label ~data:block ->
        if [%equal: Label.t] label graph.entry || [%equal: Label.t] label graph.exit
        then block
        else f (label, block))
      graph.blocks
  in
  let end_block =
    Map.find_exn graph.blocks graph.exit |> fun block -> f (graph.exit, block)
  in
  let blocks =
    blocks
    |> Map.set ~key:graph.entry ~data:start_block
    |> Map.set ~key:graph.exit ~data:end_block
  in
  { graph with blocks }
;;

let get_idoms ~jumps (graph : _ t) =
  Dominators.get_idoms ~start:graph.entry @@ to_double_graph ~jumps graph
;;

let iter_on_labels labels graph ~f:k =
  Vec.iter labels ~f:(fun label -> k (label, get_block_exn graph label))
;;

module Dfs = struct
  let reverse_postorder ~jumps graph =
    Data.Graph.Dfs.reverse_postorder
      ~start:[ graph.entry ]
      ~set:(Data.Constructors.some_hashset (module Label))
    @@ to_graph ~jumps graph
  ;;

  let preorder ~jumps graph =
    Data.Graph.Dfs.preorder
      ~start:[ graph.entry ]
      ~set:(Data.Constructors.some_hashset (module Label))
    @@ to_graph ~jumps graph
  ;;
end

module Make_gen (Block : Block_gen) = struct
  let jumps b = Block.iter_jumps b
  let to_graph g = to_graph ~jumps g
  let to_double_graph g = to_double_graph ~jumps g
  let predecessors_of_label g = predecessors_of_label ~jumps g
  let get_idoms g = get_idoms ~jumps g

  module Dfs = struct
    let reverse_postorder g = Dfs.reverse_postorder ~jumps g
    let preorder g = Dfs.preorder ~jumps g
  end
end
