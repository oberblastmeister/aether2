open! O

type 'a t =
  { span : Span.t
  ; value : 'a
  }
[@@deriving equal, compare, sexp, fields, map, iter, fold]
