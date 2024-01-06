open Core

module List = struct
  let fold = Fold.T { f = List.fold }
end

module Array = struct
  let fold = Fold.T { f = Array.fold }
end

module Set = struct
  let fold = Fold.T { f = Set.fold }
end

module Map = struct
  let fold =
    Fold.T
      { f = (fun x ~init ~f -> Map.fold x ~init ~f:(fun ~key:_ ~data z -> f z data)) }
  ;;

  let ifold =
    Fold.T
      { f = (fun x ~init ~f -> Map.fold x ~init ~f:(fun ~key ~data z -> f z (key, data)))
      }
  ;;

  let mapi m ~f = Map.mapi m ~f:(fun ~key ~data -> f (key, data))
end
