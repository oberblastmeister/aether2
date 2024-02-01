open O
open Utils.Instr_types
module NameMap = Entity.Map.Make (Name)

module Color : sig
  type t [@@deriving compare, sexp]

  val lowest : t
  val next : t -> t

  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = int [@@deriving compare, sexp]
  end

  include T

  let lowest = 0
  let next c = c + 1

  include Comparable.Make (T)
end

module Coloring = struct
  type t = (Name.t, Color.t) Entity.Map.t [@@deriving sexp_of]
end

module Allocation = struct
  type t [@@deriving sexp_of]
end

module Greedy = struct
  module Heap = Data.Array_heap.Indexed

  module IntHeap = Heap.Make (struct
      module T = struct
        type t = Name.t * int [@@deriving sexp]

        let compare x y = Int.compare (snd x) (snd y)
      end

      include T
      include Comparator.Make (T)
    end)

  let simplicial_elimination_ordering interference =
    let heap = IntHeap.create ~size:(Interference.size interference) () in
    Interference.nodes interference
    |> F.Iter.iter ~f:(fun name ->
      IntHeap.set heap ~key:(Name.Id.to_int @@ Name.to_id name) ~data:(name, 0));
    F.Iter.unfoldr
      (fun _ ->
        let open Option.Let_syntax in
        let%bind node, _weight = IntHeap.pop heap in
        Interference.neighbors interference node
        |> F.Iter.iter ~f:(fun neighbor ->
          IntHeap.modify
            heap
            ~key:(Name.Id.to_int @@ Name.to_id neighbor)
            ~f:(Tuple2.map_snd ~f:succ));
        Some (node, ()))
      ()
  ;;

  let color_with interference ordering =
    let coloring = NameMap.create () in
    ordering
    |> F.Iter.iter ~f:(fun name ->
      (* TODO: this is wrong, use the least color not used by neighbors *)
      let color =
        Interference.neighbors interference name
        |> F.Iter.map ~f:(fun neighbor ->
          NameMap.find coloring neighbor |> FC.Option.fold)
        |> F.Iter.concat
        |> F.Iter.min
        |> Option.value ~default:Color.lowest
      in
      NameMap.set coloring ~key:name ~data:color);
    coloring
  ;;

  let color interference =
    color_with interference (simplicial_elimination_ordering interference)
  ;;
end
