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

module UniqueName = struct
  type t =
    { name : string [@compare.ignore] [@equal.ignore] [@hash.ignore]
    ; unique : int
    }
  [@@deriving sexp, equal, compare, hash]

  let to_string name = name.name ^ "." ^ string_of_int name.unique
end

module Name = struct
  module T = struct
    type t =
      | Name of string
      | Unique of UniqueName.t
    [@@deriving sexp, equal, compare, hash]
  end

  include T
  module Hashtbl = Hashtbl.Make (T)
  module Hash_set = Hash_set.Make (T)
  include Comparable.Make (T)

  let of_string s = Name s

  let to_string = function
    | Name s -> s
    | Unique name -> UniqueName.to_string name
  ;;
end

module Label = struct
  module T = struct
    type t = { name : Name.t } [@@deriving equal, compare, sexp, hash] [@@unboxed]
  end

  include T
  module Hashtbl = Hashtbl.Make (T)
  module Hash_set = Hash_set.Make (T)
  include Comparable.Make (T)

  let of_string s = { name = Name.of_string s }
  let to_string { name } = Name.to_string name
end

let%expect_test "testing" =
  printf "%d" (1 + 2);
  [%expect {|3|}]
;;

let%test "testing" = [%equal: Label.t] (Label.of_string "a") (Label.of_string "a")
