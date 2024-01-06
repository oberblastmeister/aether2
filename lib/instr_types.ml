open O

module Control = struct
  type e = |
  type o = |
  type c = |
end

module SControl = struct
  type 'a t =
    | SO : Control.o t
    | SC : Control.c t
    | SE : Control.e t
end

module Name = struct
  module T = struct
    type t =
      | Name of string
      | GenName of (string * int)
    [@@deriving sexp, equal, compare, hash]
  end

  include T
  module Hashtbl = Hashtbl.Make (T)
  module Map = Map.Make (T)

  let of_string s = Name s

  let pretty = function
    | Name s -> s
    | GenName (s, i) -> s ^ "." ^ string_of_int i
  ;;
end

module Label = struct
  module T = struct
    type t = { name : Name.t } [@@deriving equal, compare, sexp, hash] [@@unboxed]
  end

  include T
  module Hashtbl = Hashtbl.Make (T)
  module Hash_set = Hash_set.Make (T)
  module Map = Map.Make (T)
  module Set = Set.Make (T)

  let of_string s = { name = Name.of_string s }
end

let%expect_test "testing" =
  printf "%d" (1 + 2);
  [%expect {|3|}]
;;

let%test "testing" = [%equal: Label.t] (Label.of_string "a") (Label.of_string "a")
