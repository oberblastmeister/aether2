open O
open Ast

val parse_string : string -> Value.t Program.t Or_error.t
val parse_string_ssa : string -> Value.t Program.t Or_error.t
val compile_string : string -> string Or_error.t
