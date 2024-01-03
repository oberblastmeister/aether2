open O

let fourth () = failwith "bruh"
let third () = fourth ()
let second () = third ()
let first _ = second ()
let%expect_test _ =
  (* first (); *)
  ()
