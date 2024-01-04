open O

type 'a t = T of ('a * 'a list) [@@deriving equal, compare, hash, sexp]

let ( |: ) x (T (y, ys)) = T (x, y :: ys)
let ( @ ) (T (x, xs)) (T (y, ys)) = T (x, (y :: xs) @ ys)
let singleton x = T (x, [])
let of_list = function x :: xs -> Some (T (x, xs)) | [] -> None
let of_list_exn xs = of_list xs |> Option.value_exn
let to_list (T (x, xs)) = x :: xs
let hd (T (x, _)) = x
let tl (T (_, xs)) = xs
let all_equal equal (T (x, xs)) = List.for_all ~f:(equal x) xs

let fold_map ~f ~combine (T (x, xs)) =
  List.fold ~init:(f x) ~f:(fun z x -> combine z (f x)) xs
