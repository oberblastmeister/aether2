type 'a t = { span : Span.t; value : 'a }
[@@deriving equal, compare, sexp, accessors]
