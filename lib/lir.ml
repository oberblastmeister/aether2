open O
include Lir_instr
include Lir_pretty

let parse s =
  let open Or_error.Let_syntax in
  let%bind fns = Lir_parse.parse s in
  let%bind fns = Lir_elaborate.elaborate fns in
  return fns
