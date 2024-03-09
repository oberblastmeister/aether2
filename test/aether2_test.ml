open Core

let some_tests =
  let open Alcotest in
  [ test_case "first" `Quick (fun () ->
      ();
      ())
  ; test_case "another" `Quick (fun () -> [%test_result: int] 1234 1234)
  ]
;;

let () =
  let open Alcotest in
  run "first" [ "sometests", some_tests ]
;;
