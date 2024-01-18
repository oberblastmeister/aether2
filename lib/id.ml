open! O

type 'k t =
  { name : string [@equal.ignore] [@compare.ignore] [@hash.ignore]
  ; unique : int
  }
[@@deriving compare, equal, hash, sexp]

let to_int { unique; _ } = unique
let global_unique = Atomic.make 0
let of_string_int name unique = { name; unique }

let of_string_global_unique name =
  of_string_int name (Atomic.fetch_and_add global_unique 1)
;;
