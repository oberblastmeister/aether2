open O
open Typerep

module First = struct
  type t [@@deriving typerep ~abstract]
end

module Second = struct
  type t [@@deriving typerep ~abstract]
end
module M = Typerep_lib

let%expect_test "first" =
  print_endline "the name";
  Typerep_lib.Typename.name First.typename_of_t |> print_endline;
  Typerep_lib.Typename.name Second.typename_of_t |> print_endline;
  [%expect {|
    the name
    lib/smoke/typerep.ml.First.t
    lib/smoke/typerep.ml.Second.t |}]
