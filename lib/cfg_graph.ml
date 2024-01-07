open O
open Instr_types

module Graph = struct
  module T = struct
    type 'b t =
      { entry : Label.t
      ; blocks : 'b Label.Map.t
      ; exit : Label.t
      }
    [@@deriving sexp_of, fields]
  end

  include T

  module Stuff = struct
    let entry = entry
    let blocks = blocks
    let exit = exit

    module Fields = Fields

    let validate graph =
      let _ = Map.find_exn graph.blocks graph.entry in
      let _ = Map.find_exn graph.blocks graph.exit in
      ()
    ;;

    let predecessors_of_label ~jumps (graph : 'b t) =
      graph.blocks
      |> Map.to_alist
      |> List.bind ~f:(fun (jumped_from, block) ->
        jumps block |> List.map ~f:(fun jumped_to -> jumped_to, jumped_from))
      |> Label.Map.of_alist_multi
    ;;

    let map_simple_order (graph : 'b t) ~(f : Label.t * 'b -> 'b) =
      let start_block =
        Map.find_exn graph.blocks graph.entry |> fun block -> f (graph.entry, block)
      in
      let blocks =
        G.Core.Map.mapi
          ~f:(fun (label, block) ->
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

    module MakeDataGraph (Block : sig
        type t [@@deriving sexp_of]

        val jumps_fold : (Label.t, t) G.Fold.t
        val jumps : t -> Label.t list
      end) :
      Data_graph.SingleEntryGraph with type t = Block.t T.t and module Node = Label =
    struct
      module Node = Label

      type t = Block.t T.t [@@deriving sexp_of]

      let entry graph = graph.entry

      let successors_fold label =
        G.Fold.premap (fun graph -> Map.find_exn graph.blocks label) Block.jumps_fold
      ;;

      let successors graph label =
        let block = Map.find_exn graph.blocks label in
        Block.jumps block
      ;;

      let predecessors_staged graph =
        let predecessors = predecessors_of_label ~jumps:Block.jumps graph in
        Staged.stage (fun label ->
          Map.find predecessors label |> Option.value ~default:[])
      ;;
    end
  end
end
