type t =
  { start : Position.t
  ; stop : Position.t
  }
[@@deriving sexp_of, equal, compare]
