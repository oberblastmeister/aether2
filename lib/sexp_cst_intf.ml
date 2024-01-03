open O

module T = struct
  type t =
    | Atom of { span : Span.t; value : string }
    | List of { span : Span.t; items : t list }
  [@@deriving equal, compare, sexp]
end

include T

module Token = struct
  type t = LParen | RParen | Atom of string [@@deriving sexp]
end

module SpannedToken = struct
  type t = { token : Token.t; span : Span.t } [@@deriving sexp]
end

module type Sexp_cst = sig
  include module type of T
  module Token : module type of Token
  module SpannedToken : module type of SpannedToken

  val tokenize : string -> SpannedToken.t list
  val parse_tokens : SpannedToken.t list -> t list Or_error.t
  val parse_tokens_single : SpannedToken.t list -> t Or_error.t
  val parse : string -> t list Or_error.t
  val parse_single : string -> t Or_error.t
  val span : t -> Span.t
end
