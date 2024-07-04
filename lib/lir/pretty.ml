open! O
open Ast
module Pretty = Sexp_lang.Pretty

type 'v context = { pretty_value : 'v -> Sexp_lang.Pretty.t }

module Context = struct
  type 'v t = 'v context

  let create ~pretty_value = { pretty_value }
end

let pretty_name_id name_id = Pretty.atom @@ Entity.Name.to_dotted_string name_id
let pretty_name (name : Name.t) = pretty_name_id name
let pretty_label (label : Label.t) = pretty_name_id label
let pretty_value (value : Value.t) = pretty_name value.name

let pretty_ty = function
  | Ty.U64 -> Pretty.(Atom "u64")
  | Ty.U1 -> Pretty.(Atom "u1")
  | Ty.I64 -> Atom "i64"
  | Void -> Atom "void"
;;

let pretty_value_typed (value : Value.t) =
  Pretty.brack_list [ pretty_name value.name; pretty_ty value.ty ]
;;

let cmp_op_to_string = function
  | Cmp_op.Gt -> "gt"
  | Ge -> "ge"
  | Eq -> "eq"
  | Lt -> "lt"
  | Le -> "le"
;;

let rec pretty_expr cx expr =
  let pretty_value = cx.pretty_value in
  match expr with
  | Expr.Bin { ty; op; v1; v2 } ->
    Pretty.(
      list
        [ Atom
            (match op with
             | Add -> "add"
             | Sub -> "sub"
             | Mul -> "mul")
        ; pretty_ty ty
        ; pretty_expr cx v1
        ; pretty_expr cx v2
        ])
  | Expr.Const { ty; const } ->
    Pretty.(list [ atom "const"; pretty_ty ty; Atom (Z.to_string_hum const) ])
  | Expr.Cmp { ty; op; v1; v2 } ->
    Pretty.(
      list
        [ atom "cmp"
        ; pretty_ty ty
        ; Atom (cmp_op_to_string op)
        ; pretty_expr cx v1
        ; pretty_expr cx v2
        ])
  | Expr.Val v -> pretty_value v
;;

let pretty_block_call cx ({ label; args } : _ Block_call.t) =
  Pretty.(
    list (List.concat [ [ pretty_label label ]; List.map ~f:(pretty_expr cx) args ]))
;;

let pretty_call cx (call : _ Call.t) =
  Pretty.(list @@ [ atom call.name ] @ List.map ~f:(pretty_expr cx) call.args)
;;

let pretty_call_with_ty cx ty call =
  Pretty.(list [ atom "call"; pretty_ty ty; pretty_call cx call ])
;;

let pretty_impure_expr cx expr =
  match expr with
  | Impure_expr.Call { ty; call } -> pretty_call_with_ty cx ty call
  | Load { ty; pointer } ->
    Pretty.(list [ Atom "load"; pretty_ty ty; pretty_expr cx pointer ])
  | _ -> todo [%here]
;;

let pretty_instr_control cx i =
  match i with
  | Control_instr.Jump v -> Pretty.(list [ Atom "jump"; pretty_block_call cx v ])
  | Control_instr.CondJump (v, j1, j2) ->
    Pretty.(
      list
        [ Atom "cond_jump"
        ; pretty_expr cx v
        ; pretty_block_call cx j1
        ; pretty_block_call cx j2
        ])
  | Control_instr.Ret v ->
    Pretty.(list ([ Atom "ret" ] @ (Option.map ~f:(pretty_expr cx) v |> Option.to_list)))
;;

let pretty_instr cx i =
  match i with
  | Instr.Assign { dst; expr } ->
    Pretty.(list [ Atom "set"; pretty_value dst; pretty_expr cx expr ])
  | Instr.ImpureAssign { dst; expr } ->
    Pretty.(list [ Atom "set"; pretty_value dst; pretty_impure_expr cx expr ])
  | VoidCall call -> pretty_call_with_ty cx Void call
  | Store { ty; pointer; expr } ->
    Pretty.(
      list [ Atom "store"; pretty_ty ty; pretty_expr cx pointer; pretty_expr cx expr ])
;;

let pretty_block cx (label : Label.t) (block : _ Block.t) =
  Pretty.(
    list
    @@ List.concat
         [ [ Atom "block"
           ; list ([ pretty_label label ] @ List.map ~f:pretty_value_typed block.entry)
           ; Ann IndentLine
           ]
         ; List.map ~f:(pretty_instr cx) block.body |> List_ext.end_with ~sep:(Ann Line)
         ; [ pretty_instr_control cx block.exit ]
         ])
;;

let pretty_graph cx (graph : _ Graph.t) =
  Cfg.Graph.Dfs.reverse_postorder ~jumps:Block.iter_jumps graph
  |> Vec.to_list
  |> List.map ~f:(fun label ->
    let block = Cfg.Graph.find_exn label graph in
    pretty_block cx label block)
  |> List.intersperse ~sep:(Ann Line)
;;

let pretty_function cx (fn : _ Function.t) =
  Pretty.(
    list
    @@ List.concat
         [ [ Atom "define"
           ; list ([ Atom fn.name ] @ List.map ~f:pretty_value_typed fn.ty.params)
           ; pretty_ty fn.ty.return
           ]
         ; [ Ann IndentLine ]
         ; pretty_graph cx fn.graph
         ])
;;

let pretty' cx (program : _ Program.t) =
  program.funcs
  |> List.map ~f:(pretty_function cx)
  |> List.map ~f:Pretty.to_string
  |> String.concat ~sep:"\n\n"
;;

let pretty program = pretty' { pretty_value } program
