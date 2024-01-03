open O

let%expect_test _ =
  let open Univ_map in
  let bruh = Key.create ~name:"bruh" [%sexp_of: int] in
  ()
