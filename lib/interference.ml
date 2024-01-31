open O
open Utils.Instr_types
module NameMap = Entity.Map.Make (Name)

type t = (Name.t, Name.t Hash_set.t) Entity.Map.t [@@deriving sexp_of]

let create () = NameMap.create ()

let add_directed_edge g n1 n2 =
  NameMap.update g n1 ~f:(function
    | None ->
      let set = Hash_set.create (module Name) in
      Hash_set.add set n2;
      set
    | Some set ->
      Hash_set.add set n2;
      set)
;;

let add_edge g n1 n2 =
  add_directed_edge g n1 n2;
  add_directed_edge g n2 n1
;;

let neighbors g n = NameMap.find g n |> FC.Option.fold @> FC.Hash_set.fold
