type t =
  | JE
  | JNE
  | JB
  | JBE
  | JA
[@@deriving equal, compare, sexp, hash, variants]
