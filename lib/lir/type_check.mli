open O
open Ast

type error = [ `LirTypeCheckError of Sexp.t ] [@@deriving sexp_of]

val run : Value.t Program.t -> (unit, [> error ]) result
