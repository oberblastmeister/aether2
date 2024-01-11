open Lir_instr

module Context : sig
  type 'v t = { pretty_value : 'v -> Sexp_pretty.t }
end

val pretty' : 'a Context.t -> 'a Program.t -> string
val pretty_value : Value.t -> Sexp_pretty.t
val pretty_instr_op : 'a Context.t -> 'a InstrOp.t -> Sexp_pretty.t
val pretty : Value.t Program.t -> string
