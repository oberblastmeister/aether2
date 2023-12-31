let some_tests =
  let open Alcotest in
  [
    test_case "first" `Quick (fun () ->
        ();
        ());
  ]

let () =
  let open Alcotest in
  run "first" [ ("sometests", some_tests) ]
