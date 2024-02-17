open Core

module List = struct
  let fold x k = List.iter x ~f:k
end

module Array = struct
  let fold x k = Array.iter x ~f:k
end

module Option = struct
  let fold x k = Option.iter x ~f:k
end

module Set = struct
  let fold x k = Set.iter x ~f:k
  let iter s f = Set.iter s ~f
end

module Map = struct
  let fold x k = Map.iter x ~f:k
  let foldi x k = Map.iteri x ~f:(fun ~key ~data -> k (key, data))
  let mapi m ~f = Map.mapi m ~f:(fun ~key ~data -> f (key, data))
  let map m ~f = Map.mapi m ~f:(fun ~key:_ ~data -> f data)
end

module Hash_set = struct
  let fold x k = Hash_set.iter x ~f:k
end

module Hashtbl = struct
  let of_iter key i =
    let t = Hashtbl.create key in
    i (fun (k, v) -> Hashtbl.set t ~key:k ~data:v);
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
