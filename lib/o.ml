module Core_ext = struct
  include Core

  module Hashtbl = struct
    include Hashtbl

    (** testing a*)
    let of_iter key i =
      let t = Hashtbl.create key in
      i ~f:(fun (k, v) -> Hashtbl.set t ~key:k ~data:v);
      t
    ;;
  end

  module Map = struct
    include Map

    let iter_kv t ~f = Map.iteri t ~f:(fun ~key ~data -> f (key, data))

    let of_iter cmp it =
      Map.of_iteri cmp ~iteri:(fun ~f -> it ~f:(fun (key, data) -> f ~key ~data))
    ;;
  end

  module Either = struct
    include Either

    let iter_first e k =
      match e with
      | First x -> k x
      | _ -> ()
    ;;

    let iter_second e k =
      match e with
      | Second x -> k x
      | _ -> ()
    ;;
  end

  module Tuple2 = struct
    include Tuple2

    let iter_both (x, y) ~f =
      f x;
      f y
    ;;
  end
end

include Core_ext
include Folds.O
module F = Folds
module Data = Aether_data
module Vec = Data.Vec
module List1 = Data.List1
module Entity = Aether_entity
module Ppx_log_syntax = Logger.Ppx_log_syntax

module Z = struct
  module Z' = struct
    include Z

    let of_string_opt s =
      match Z.of_string s with
      | exception _ -> None
      | z -> Some z
    ;;

    let to_string_hum = Z.to_string
    let sexp_of_t z = Sexp.Atom (Z.to_string z)

    let t_of_sexp = function
      | Sexp.Atom s ->
        (match of_string_opt s with
         | Some z -> z
         | None -> raise_s [%message "Z.t_of_sexp: invalid Z" (s : string)])
      | _ -> raise_s [%message "Z.t_of_sexp: invalid Sexp"]
    ;;
  end

  include Z'
  include Comparable.Make (Z')
end

let ( |- ) x f =
  f x;
  x
;;

(* CPS let operator *)
let ( let@ ) f x = f x
let todo location = raise_s [%message "TODO" (location : Source_code_position.t)]
let assert_s b sexp = if b then () else raise_s sexp
