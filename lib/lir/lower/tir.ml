open O
module Lir = Types

module Value = struct
  type t =
    | V of Lir.Value.t
    | I of t Lir.Instr.t
  [@@deriving sexp_of]

  let rec uses_fold t k =
    match t with
    | V v -> k v
    | I i -> (Lir.Instr.uses_fold @> uses_fold) i k
  ;;
end

include Instantiate.Instantiate (Value)

let rec pretty_value = function
  | Value.V v -> Pretty.pretty_value v
  | Value.I (Lir.Instr.Assign { expr; _ }) ->
    Pretty.pretty_expr (Pretty.Context.create ~pretty_value) expr
  | Value.I _ -> failwith "should be assign instruction"
;;

let pretty = Pretty.pretty' (Pretty.Context.create ~pretty_value)
