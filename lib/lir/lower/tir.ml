open O
module Lir = Types

module Value = struct
  type t =
    | V of Lir.Value.t
    | I of
        { dst : Lir.Value.t
        ; expr : t Lir.Expr.t
        }
  [@@deriving sexp_of]

  let to_value = function
    | V v -> v
    | I { dst; _ } -> dst
  ;;

  let get_name = function
    | V v -> v.name
    | I { dst; _ } -> dst.name
  ;;

  let get_ty = function
    | V v -> v.ty
    | I { dst; _ } -> dst.ty
  ;;

  let rec iter_uses t ~f =
    match t with
    | V v -> f v
    | I { expr; _ } -> (Lir.Expr.iter_uses @> iter_uses) expr ~f
  ;;
end

include Instantiate.Instantiate (Value)

let rec pretty_value = function
  | Value.V v -> Pretty.pretty_value v
  | Value.I { expr; _ } -> Pretty.pretty_expr (Pretty.Context.create ~pretty_value) expr
;;

let pretty = Pretty.pretty' (Pretty.Context.create ~pretty_value)
