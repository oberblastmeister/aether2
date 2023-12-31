open O

module Control = struct
  type e
  type o
  type c
end

module SControl = struct
  type 'a t = SO : Control.o t | SC : Control.c t | SE : Control.e t
end

module Label = struct
  module T = struct
    type t = T of string [@@deriving show, equal, compare, sexp, hash]
  end

  include T
  module Hashtbl = Hashtbl.Make (T)
  module Map = Map.Make (T)
  module Set = Set.Make (T)
end

module Name = struct
  module T = struct
    type t = Name of string | GenName of (string * int)
    [@@deriving sexp, equal, compare, hash]
  end

  include T
  module Hashtbl = Hashtbl.Make (T)
  module Map = Map.Make (T)
end

let%expect_test "testing" =
  printf "%d" (1 + 2);
  [%expect {|3|}]

let%test "testing" = [%equal: Label.t] (Label.T "a") (Label.T "a")
