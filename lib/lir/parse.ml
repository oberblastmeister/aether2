open! O
open Ast
module Parser = Sexp_lang.Parser
module Intern_table = Utils.Intern_table.String_intern

type state =
  { label_intern : Label.key Intern_table.t
  ; name_intern : Name.key Intern_table.t
  }

let create_state () =
  { label_intern = Intern_table.create (); name_intern = Intern_table.create () }
;;

let parse_ty =
  Parser.atom (function
    | "i64" -> Ty.I64
    | "i1" -> Ty.I1
    | "void" -> Ty.Void
    | "ptr" -> Ty.Ptr
    | ty -> Parser.parse_error [%message "unknown type" (ty : string)])
;;

let parse_label st = Parser.atom (fun s -> Intern_table.name_of_key st.label_intern s)
let parse_var st = Parser.atom (fun s -> Intern_table.name_of_key st.name_intern s)
let parse_int32 = Parser.atom Int32.of_string

let parse_lit s =
  Parser.atom (fun s' ->
    if [%equal: string] s s'
    then ()
    else Parser.parse_error [%message "expected literal" ~s ~got:s'])
;;

let parse_value st sexp =
  let@ xs = Parser.list_ref sexp in
  let name = Parser.item xs (parse_var st) in
  let _ = Parser.item xs (parse_lit ":") in
  let ty = Parser.item xs parse_ty in
  ({ name; ty } : Value.t)
;;

let parse_ident = Parser.atom Fn.id

let parse_cmp_op = function
  | "gt" -> Cmp_op.Gt
  | "ge" -> Cmp_op.Ge
  | "eq" -> Cmp_op.Eq
  | "lt" -> Cmp_op.Lt
  | "le" -> Cmp_op.Le
  | s -> Parser.parse_error [%message "unknown cmp op" ~op:s]
;;

let parse_keywords sexps_ref =
  let rec go acc sexps =
    match sexps with
    | [] -> acc, []
    | Sexp_lang.Cst.Keyword { value = name; _ } :: sexp :: sexps ->
      go
        (Map.update acc name ~f:(function
          | None -> List1.(singleton sexp)
          | Some values -> List1.(sexp |: values)))
        sexps
    | _ -> acc, sexps
  in
  let acc, sexps = go String.Map.empty !sexps_ref in
  sexps_ref := sexps;
  acc
;;

let take_keywords name keywords =
  ( Map.remove keywords name
  , Map.find keywords name |> Option.map ~f:List1.to_list |> Option.value ~default:[] )
;;

let rec parse_expr st sexp =
  match sexp with
  | Sexp_lang.Cst.Atom s ->
    Expr.Val { ty = None; v = Intern_table.name_of_key st.name_intern s.value }
  | Keyword { value; _ } -> Parser.parse_error [%message "did not expect keyword" value]
  | List list ->
    let xs = ref list.items in
    let name = Parser.item xs parse_ident in
    (match name with
     | "val" ->
       let ty = Parser.item xs parse_ty in
       let v = Parser.item xs (parse_var st) in
       Expr.Val { ty = Some ty; v }
     | "const" ->
       let ty = Parser.item xs parse_ty in
       let const =
         Parser.item xs
         @@ Parser.atom (fun s ->
           Z.of_string_opt s
           |> Option.value_or_thunk ~default:(fun () ->
             Parser.parse_error [%message "couldn't parse number" ~s]))
       in
       Expr.Const { ty; const }
     | "icmp" ->
       let ty = Parser.item xs parse_ty in
       let op = Parser.item xs @@ Parser.atom parse_cmp_op in
       let v1 = Parser.item xs (parse_expr st) in
       let v2 = Parser.item xs (parse_expr st) in
       Expr.Cmp { ty; op; signed = Signed; v1; v2 }
     | "ucmp" ->
       let ty = Parser.item xs parse_ty in
       let op = Parser.item xs @@ Parser.atom parse_cmp_op in
       let v1 = Parser.item xs (parse_expr st) in
       let v2 = Parser.item xs (parse_expr st) in
       Expr.Cmp { ty; op; signed = Unsigned; v1; v2 }
     | "add" -> parse_bin Bin_op.Add st xs
     | "sub" -> parse_bin Sub st xs
     | "mul" -> parse_bin Mul st xs
     | _ -> Parser.parse_error [%message "unknown op" ~name])

and parse_impure_expr st sexp =
  let@ xs = Parser.list_ref sexp in
  let name = Parser.item xs parse_ident in
  match name with
  | "call" ->
    let ty = Parser.item xs parse_ty in
    let@ call = Parser.item xs in
    let call = parse_call st call in
    Impure_expr.Call { ty; call }
  | "load" ->
    let ty = Parser.item xs parse_ty in
    let ptr = Parser.item xs (parse_expr st) in
    Load { ty; ptr }
  | "alloca" ->
    let size = Parser.item xs parse_int32 in
    Alloca { size }
  | "idiv" ->
    let ty = Parser.item xs parse_ty in
    let v1 = Parser.item xs (parse_expr st) in
    let v2 = Parser.item xs (parse_expr st) in
    Idiv { ty; v1; v2 }
  | "udiv" ->
    let ty = Parser.item xs parse_ty in
    let v1 = Parser.item xs (parse_expr st) in
    let v2 = Parser.item xs (parse_expr st) in
    Udiv { ty; v1; v2 }
  | name -> raise_s [%message "unknown impure expr" (name : string)]

and parse_call st sexp =
  let@ xs = Parser.list_ref sexp in
  let name = Parser.item xs (Parser.atom Fn.id) in
  let args = Parser.rest !xs (parse_expr st) in
  Call.{ name; args }

and parse_bin op st xs =
  let ty = Parser.item xs parse_ty in
  let v1 = Parser.item xs (parse_expr st) in
  let v2 = Parser.item xs (parse_expr st) in
  Expr.Bin { ty; op; v1; v2 }

and parse_block_call st sexp =
  let@ xs = Parser.list_ref sexp in
  let label = Parser.item xs (parse_label st) in
  let args = Parser.rest !xs (parse_expr st) in
  ({ label; args } : _ Block_call.t)
;;

let parse_instr st sexp =
  let instr_o i : _ Some_instr.t = Some_instr.T (Generic_instr.Instr i) in
  let instr_c instr_control : _ Some_instr.t =
    Some_instr.T (Generic_instr.Control instr_control)
  in
  let@ xs = Parser.list_ref sexp in
  let instr_name = Parser.item xs parse_ident in
  match instr_name with
  | "set" ->
    let name = Parser.item xs (parse_var st) in
    (* TODO: handle single values here *)
    let expr = Parser.item xs (Parser.either (parse_expr st) (parse_impure_expr st)) in
    (match expr with
     | First expr ->
       let ty =
         Expr.get_ty_with'
           (fun ty _ ->
             Option.value_or_thunk ty ~default:(fun () ->
               Parser.parse_error [%message "set with val must have ty"]))
           expr
       in
       instr_o (Assign { dst = { Value.name; ty }; expr })
     | Second expr ->
       let ty = Impure_expr.get_ty expr in
       instr_o (ImpureAssign { dst = { Value.name; ty }; expr }))
  | "call" ->
    let _ = Parser.item xs (parse_lit "void") in
    let@ call = Parser.item xs in
    let call = parse_call st call in
    Some_instr.T (Instr (VoidCall call))
  | "ret" ->
    let v = Parser.optional_item !xs (parse_expr st) in
    instr_c (Control_instr.Ret v)
  | "jump" ->
    let j = Parser.item xs (parse_block_call st) in
    instr_c (Control_instr.Jump j)
  | "cond_jump" ->
    let v = Parser.item xs (parse_expr st) in
    let j1 = Parser.item xs (parse_block_call st) in
    let j2 = Parser.item xs (parse_block_call st) in
    instr_c (Control_instr.CondJump (v, j1, j2))
  | "store" ->
    let ty = Parser.item xs parse_ty in
    let ptr = Parser.item xs (parse_expr st) in
    let expr = Parser.item xs (parse_expr st) in
    instr_o (Store { ty; ptr; expr })
  | _ -> Parser.parse_error [%message "unknown instruction" ~name:instr_name]
;;

let parse_block st sexp =
  let@ xs = Parser.list_ref sexp in
  Parser.item xs @@ parse_lit "block";
  let label, args =
    let@ x = Parser.item xs in
    let@ xs = Parser.list_ref x in
    let label = Parser.item xs (parse_label st) in
    let args = Parser.rest !xs (parse_value st) in
    label, args
  in
  let instrs = Parser.rest !xs (parse_instr st) in
  let instrs, last_instr =
    List_ext.unsnoc_list instrs
    |> Option.value_or_thunk ~default:(fun () ->
      Parser.parse_error [%message "cannot have empty block"])
  in
  let instrs =
    List.map instrs ~f:(fun (Some_instr.T i) ->
      match i with
      | Generic_instr.Instr instr -> instr
      | _ ->
        Parser.parse_error
          [%message
            "instructions before the end must be op instrs"
              ~got:(Generic_instr.sexp_of_t Name.sexp_of_t i : Sexp.t)])
  in
  let last_instr =
    let (Some_instr.T i) = last_instr in
    match i with
    | Generic_instr.Control i -> i
    | _ ->
      Parser.parse_error
        [%message
          "last instruction must be control instruction"
            ~got:(Generic_instr.sexp_of_t Name.sexp_of_t i : Sexp.t)]
  in
  label, ({ entry = args; body = instrs; exit = last_instr } : _ Block.t)
;;

let parse_graph st xs =
  let blocks = Parser.rest xs (parse_block st) in
  match blocks with
  | (label, _) :: _ ->
    Cfg.Graph.of_alist ~entry:label ~exit:(List.last_exn blocks |> fst) blocks
  | _ -> Parser.parse_error [%message "graph must have at least one block"]
;;

let parse_linkage = function
  | "export" -> Linkage.Export
  | "preemptible" -> Linkage.Preemptible
  | "local" -> Linkage.Local
  | linkage -> Parser.parse_error [%message "invalid linkage" linkage]
;;

let parse_single xs msg =
  match xs with
  | [ x ] -> x
  | _ -> Parser.parse_error [%message "more than one of " (msg : Sexp.t)]
;;

let parse_some o msg =
  match o with
  | Some x -> x
  | None -> Parser.parse_error [%message "expected Some " (msg : Sexp.t)]
;;

let parse_global_data sexp = todo [%here]

let parse_decl sexp =
  let st = create_state () in
  let@ xs = Parser.list_ref sexp in
  let decl_name = Parser.item xs @@ Parser.string in
  match decl_name with
  | "define" ->
    let keywords = parse_keywords xs in
    let _keywords, linkage = take_keywords "linkage" keywords in
    let linkage =
      List.hd linkage
      |> Option.map ~f:(fun sexp -> Parser.atom parse_linkage sexp)
      |> Option.value ~default:Linkage.Export
    in
    let@ name_or_list = Parser.item xs in
    (match name_or_list with
     | Keyword { value; _ } ->
       Parser.parse_error [%message "expected name or list" (value : string)]
     | Atom { value; _ } -> todo [%here]
     | List { items; _ } ->
       let xs = ref items in
       let name, params =
         let xs = ref items in
         let name = Parser.item xs parse_ident in
         let params = Parser.rest !xs (parse_value st) in
         name, params
       in
       let _ = Parser.item xs (parse_lit ":") in
       let return = Parser.item xs parse_ty in
       let ty = { Named_function_ty.params; return } in
       (match !xs with
        | [] -> Decl.Func_def { name; linkage; ty }
        | xs ->
          let graph = parse_graph st xs in
          Decl.Func
            { name
            ; linkage
            ; ty
            ; graph
            ; unique_label = Intern_table.get_next_id st.label_intern
            ; unique_name = Intern_table.get_next_id st.name_intern
            }))
  | _ -> raise_s [%message "didn't know how to parse declaration" decl_name]
;;

let parse_decls xs = Parser.rest xs parse_decl
(* let empty_program : _ Module.t = { funcs = []; externs = [] } *)

(* let reverse_program (program : _ Module.t) =
  { program with funcs = List.rev program.funcs; externs = List.rev program.externs }
;; *)

let parse_program xs =
  let decls = parse_decls xs in
  { Module.decls }
;;

let parse s =
  let open Result.Let_syntax in
  let%bind sexp = Sexp_lang.Cst.parse s in
  let%bind program =
    Sexp_lang.Parser.run (fun () -> parse_program sexp)
    |> Result.map_error ~f:Parser.Error.to_error
  in
  return program
;;

let%expect_test _ =
  let s =
    {|
(define (testing (first u64) (second u64)) u64
    (block (first (arg u64))
        (set x (add u64 first second))
        (ret)))
  |}
  in
  let fns = parse s |> Or_error.ok_exn in
  print_s [%sexp (fns : _ Module.t)];
  ();
  [%expect
    {|
    ((funcs
      (((name testing)
        (graph
         ((entry first.0)
          (blocks
           ((first.0
             ((entry (((name arg.2) (ty U64))))
              (body
               ((Assign (dst ((name x.3) (ty U64)))
                 (expr (Bin (ty U64) (op Add) (v1 (Val _)) (v2 (Val _)))))))
              (exit (Ret ()))))))
          (exit first.0)))
        (ty
         ((params (((name first.0) (ty U64)) ((name second.1) (ty U64))))
          (return U64)))
        (unique_label 1) (unique_name 4))))
     (externs ())) |}]
;;
