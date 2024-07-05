open! O

type ann =
  | Line
  | IndentLine
[@@deriving equal, compare, sexp]

module Delim = struct
  type t =
    | Paren
    | Brack
    | Brace
  [@@deriving sexp, equal, compare, sexp]
end

module T = struct
  type t =
    | List of t list * Delim.t
    | Atom of string
    | Keyword of string
    | Ann of ann
  [@@deriving equal, compare, sexp]
end

include T

module type Intf = sig
  include module type of T

  val atom : string -> t
  val list : t list -> t
  val brack_list : t list -> t
  val brace_list : t list -> t
  val char_of_open_delim : Delim.t -> char
  val char_of_close_delim : Delim.t -> char
  val to_string : t -> string
end
