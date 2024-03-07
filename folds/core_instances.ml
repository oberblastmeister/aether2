open Core

module Map = struct
  let mapi m ~f = Map.mapi m ~f:(fun ~key ~data -> f (key, data))
  let map m ~f = Map.mapi m ~f:(fun ~key:_ ~data -> f data)
end

module Hashtbl = struct
  let of_iter key i =
    let t = Hashtbl.create key in
    i ~f:(fun (k, v) -> Hashtbl.set t ~key:k ~data:v);
    t
  ;;
end

module Either = struct
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
  let fold_both (x, y) k =
    k x;
    k y
  ;;
end
