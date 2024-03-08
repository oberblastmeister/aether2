open O
open Utils.Instr_types

type t = (Name.t, Name.Set.t) Entity.Map.t [@@deriving sexp_of]

let create () = Name.Table.create ()
let add_node g n = Name.Table.update g n ~f:(Option.value ~default:Name.Set.empty)

let add_directed_edge g n1 n2 =
  if Name.(n1 <> n2)
  then (
    Name.Table.update g n1 ~f:(function
      | None -> Name.Set.singleton n2
      | Some set -> Set.add set n2);
    ())
;;

let add_edge g n1 n2 =
  add_directed_edge g n1 n2;
  add_directed_edge g n2 n1
;;

let neighbors g n = Name.Table.find g n |> Option.iter @> Set.iter
let size = Entity.Map.size
let nodes t = Entity.Map.iteri t |> F.Iter.map ~f:fst
