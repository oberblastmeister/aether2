open! O

module Error : sig
  type t =
    { span : Span.t
    ; message : Sexp.t
    }
  [@@deriving equal, compare, sexp]

  val to_error : t -> Error.t
end

val run : (unit -> 'a) -> ('a, Error.t) Result.t
val with_span : Span.t -> (unit -> 'a) -> 'a
val parse_error : Sexp.t -> 'a
val atom : (string -> 'a) -> Cst.t -> 'a
val string : Cst.t -> string
val list : Cst.t -> (Cst.t list -> 'a) -> 'a
val list_ref : Cst.t -> (Cst.t list ref -> 'a) -> 'a
val item : Cst.t list ref -> (Cst.t -> 'a) -> 'a
val optional_item : Cst.t list -> (Cst.t -> 'a) -> 'a option
val rest : Cst.t list -> (Cst.t -> 'a) -> 'a list
