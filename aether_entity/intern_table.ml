open Core

(* TODO: make this a functor *)

type 'k t =
  { mutable id : 'k Id.t
  ; id_of_string : 'k Id.t String.Table.t
  }
[@@deriving sexp_of]

let create ?(next_id = Id.initial) _ =
  { id = next_id; id_of_string = String.Table.create () }
;;

let id_of_string t s =
  match Hashtbl.find t.id_of_string s with
  | Some id -> id
  | None ->
    let id = t.id in
    t.id <- Id.next id;
    Hashtbl.add_exn t.id_of_string ~key:s ~data:id;
    id
;;

let name_of_string t s = Name.create s (id_of_string t s)
let get_next_id t = t.id
