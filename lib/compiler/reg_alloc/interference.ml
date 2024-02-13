open O
open Utils.Instr_types


type t = (Name.t, Name.t Hash_set.t) Entity.Map.t [@@deriving sexp_of]

let create () = Name.Table.create ()

let add_node g n =
  Name.Table.update g n ~f:(Option.value ~default:(Hash_set.create (module Name)))
;;

let add_directed_edge g n1 n2 =
  Name.Table.update g n1 ~f:(function
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

let neighbors g n = Name.Table.find g n |> FC.Option.fold @> FC.Hash_set.fold
let size = Entity.Map.size
let nodes t = Entity.Map.to_iteri t |> F.Iter.map ~f:fst
