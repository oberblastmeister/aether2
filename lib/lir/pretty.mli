open Instr

module Context : sig
  type 'v t = { pretty_value : 'v -> Sexp_lang.Pretty.t }
end

val pretty' : 'a Context.t -> 'a Program.t -> string
val pretty_value : Value.t -> Sexp_lang.Pretty.t
val pretty_instr : 'a Context.t -> 'a Instr.t -> Sexp_lang.Pretty.t
val pretty : Value.t Program.t -> string
