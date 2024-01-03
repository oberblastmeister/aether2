open O

let rec unsnoc_list xs =
  let open Option.Let_syntax in
  match xs with
  | x :: [] -> Some ([], x)
  | x :: xs ->
      let%bind ys, y = unsnoc_list xs in
      Some (x :: ys, y)
  | [] -> None
