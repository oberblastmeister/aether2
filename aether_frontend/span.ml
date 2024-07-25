type t =
  { start : Position.t
  ; stop : Position.t
  }
[@@deriving sexp_of, equal, compare]

let garbage = { start = { line = 0; col = 0 }; stop = { line = 0; col = 0 } }
