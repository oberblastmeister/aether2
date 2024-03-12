open! O
open Ast
module Pretty = Sexp_lang.Pretty

module Context = struct
  type 'v t = { pretty_value : 'v -> Sexp_lang.Pretty.t }

  let create ~pretty_value = { pretty_value }
end

let pretty_name_id name_id = Pretty.atom @@ Entity.Name.to_dotted_string name_id
let pretty_name (name : Name.t) = pretty_name_id name
let pretty_label (label : Label.t) = pretty_name_id label
let pretty_value (value : Value.t) = pretty_name value.name

let pretty_ty = function
  | Ty.U64 -> Pretty.(Atom "u64")
  | Ty.U1 -> Pretty.(Atom "u1")
;;

let pretty_value_typed (value : Value.t) =
  Pretty.List [ pretty_name value.name; pretty_ty value.ty ]
;;

let cmp_op_to_string = function
  | Cmp_op.Gt -> "gt"
;;

let pretty_expr cx =
  let pretty_value = cx.Context.pretty_value in
  function
  | Expr.Bin { ty; op; v1; v2 } ->
    Pretty.(
      List
        [ (match op with
           | Add -> Atom "add"
           | _ -> todo [%here])
        ; pretty_ty ty
        ; pretty_value v1
        ; pretty_value v2
        ])
  | Expr.Const { ty; const } ->
    Pretty.(List [ Atom "const"; pretty_ty ty; Atom (Int64.to_string_hum const) ])
  | Expr.Cmp { ty; op; v1; v2 } ->
    Pretty.(
      List
        [ Atom "cmp"
        ; pretty_ty ty
        ; Atom (cmp_op_to_string op)
        ; pretty_value v1
        ; pretty_value v2
        ])
  | Expr.Val { ty; v } -> Pretty.(List [ pretty_ty ty; pretty_value v ])
  | _ -> failwith "don't know how to print"
;;

let pretty_block_call cx ({ label; args } : _ Block_call.t) =
  let pretty_value = cx.Context.pretty_value in
  Pretty.(List (List.concat [ [ pretty_label label ]; List.map ~f:pretty_value args ]))
;;

let pretty_instr_control cx i =
  let pretty_value = cx.Context.pretty_value in
  match i with
  | Control_instr.Jump v -> Pretty.(List [ Atom "jump"; pretty_block_call cx v ])
  | Control_instr.CondJump (v, j1, j2) ->
    Pretty.(
      List
        [ Atom "cond_jump"
        ; pretty_value v
        ; pretty_block_call cx j1
        ; pretty_block_call cx j2
        ])
  | Control_instr.Ret v ->
    Pretty.(List ([ Atom "ret" ] @ (Option.map ~f:pretty_value v |> Option.to_list)))
;;

let pretty_instr cx i =
  match i with
  | Instr.Assign { dst; expr } ->
    Pretty.(List [ Atom "set"; pretty_value dst; pretty_expr cx expr ])
  | _ -> todo [%here]
;;

let pretty_block cx (label : Label.t) (block : _ Block.t) =
  Pretty.(
    list
    @@ List.concat
         [ [ Atom "label"
           ; List ([ pretty_label label ] @ List.map ~f:pretty_value_typed block.entry)
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
           ; List ([ Atom fn.name ] @ List.map ~f:pretty_value_typed fn.ty.params)
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

let pretty program = pretty' { Context.pretty_value } program
