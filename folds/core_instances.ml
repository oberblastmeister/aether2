open Core

module List = struct
  let fold x k = List.iter x ~f:k
end

module Array = struct
  let fold x k = Array.iter x ~f:k
end

module Set = struct
  let fold x k = Set.iter x ~f:k
end

module Map = struct
  let fold x k = Map.iter x ~f:k
  let foldi x k = Map.iteri x ~f:(fun ~key ~data -> k (key, data))
  let mapi m ~f = Map.mapi m ~f:(fun ~key ~data -> f (key, data))
end
