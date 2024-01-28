open O
open Types

type state =
  | Once
  | Multiple
[@@deriving equal, sexp_of]

type t [@@deriving sexp_of]

val create : Vir.Function.t -> Instr_of_value.t -> t
val find : t -> Types.Value.t -> state
