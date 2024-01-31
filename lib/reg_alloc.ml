open O
open Utils.Instr_types
module NameMap = Entity.Map.Make (Name)

module Color : sig
  type t [@@deriving sexp_of]

  val lowest : t
  val next : t -> t
end = struct
  type t = int [@@deriving sexp_of]

  let lowest = 0
  let next c = c + 1
end

module Coloring = struct
  type t = (Name.t, Color.t) Entity.Map.t [@@deriving sexp_of]
end

module Allocation = struct
  type t [@@deriving sexp_of]
end

module Greedy = struct
  let color_with interference ordering =
    let coloring = NameMap.create () in
    ordering (fun name ->
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
  
  (* let simplicial_elimination_ordering  *)
end
