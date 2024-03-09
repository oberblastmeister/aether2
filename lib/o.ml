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

let ( |- ) x f =
  f x;
  x
;;

let todo location = raise_s [%message "TODO" (location : Source_code_position.t)]
