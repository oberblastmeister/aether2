open O
open Ast

type emit =
  | Lir
  | Tir
  | X86
  | Asm
[@@deriving sexp_of]

val parse_string : string -> Value.t Program.t Or_error.t
val parse_string_ssa : string -> Value.t Program.t Or_error.t
val compile_string : ?emit:emit -> string -> string Or_error.t
