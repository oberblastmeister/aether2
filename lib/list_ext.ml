open O

let rec unsnoc_list xs =
  let open Option.Let_syntax in
  match xs with
  | x :: [] -> Some ([], x)
  | x :: xs ->
    let%bind ys, y = unsnoc_list xs in
    Some (x :: ys, y)
  | [] -> None
;;

let end_with ~sep = function
  | _ :: _ as xs -> List.intersperse xs ~sep @ [ sep ]
  | [] -> []
;;

let start_with ~sep = function
  | _ :: _ as xs -> sep :: List.intersperse xs ~sep
  | [] -> []
;;
