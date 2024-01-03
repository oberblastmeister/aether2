open O

module Error : sig
  type t = { span : Span.t; message : Sexp.t } [@@deriving equal, compare, sexp]
  val to_error : t -> Error.t
end

val run : (unit -> 'a) -> ('a, Error.t) Result.t
val with_span : Span.t -> (unit -> 'a) -> 'a
val parse_error : Sexp.t -> 'a
val atom : (string -> 'a) -> Sexp_cst.t -> 'a
val list : (Sexp_cst.t list -> 'a) -> Sexp_cst.t -> 'a
val list_ref : (Sexp_cst.t list ref -> 'a) -> Sexp_cst.t -> 'a
val item : Sexp_cst.t list ref -> (Sexp_cst.t -> 'a) -> 'a
val optional_item : Sexp_cst.t list -> (Sexp_cst.t -> 'a) -> 'a option
val rest : Sexp_cst.t list -> (Sexp_cst.t -> 'a) -> 'a list
