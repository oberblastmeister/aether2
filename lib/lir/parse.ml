open! O
open Ast
module Parser = Sexp_lang.Parser
module Intern_table = Entity.Intern_table

type state =
  { label_intern : Label.key Intern_table.t
  ; name_intern : Name.key Intern_table.t
  }

let create_state () =
  { label_intern = Intern_table.create (module Label)
  ; name_intern = Intern_table.create (module Name)
  }
;;

let parse_ty =
  Parser.atom (function
    | "u64" -> Ty.U64
    | "u1" -> Ty.U1
    | "void" -> Ty.Void
    | _ -> Parser.parse_error [%message "unknown type"])
;;

let parse_label st = Parser.atom (fun s -> Intern_table.name_of_string st.label_intern s)
let parse_var st = Parser.atom (fun s -> Intern_table.name_of_string st.name_intern s)

let parse_value st sexp =
  let@ xs = Parser.list_ref sexp in
  let name = Parser.item xs (parse_var st) in
  let ty = Parser.item xs parse_ty in
  ({ name; ty } : Value.t)
;;

let parse_ident = Parser.atom Fn.id

let parse_cmp_op = function
  | "gt" -> Cmp_op.Gt
  | s -> Parser.parse_error [%message "unknown cmp op" ~op:s]
;;

let rec parse_expr st sexp =
  match sexp with
  | Sexp_lang.Cst.Atom s -> Expr.Val (Intern_table.name_of_string st.name_intern s.value)
  | List list ->
    let xs = ref list.items in
    let name = Parser.item xs parse_ident in
    (match name with
     (* | "val" ->
       let ty = Parser.item xs parse_ty in
       let v = Parser.item xs (parse_var st) in
       Expr.Val { v } *)
     | "const" ->
       let ty = Parser.item xs parse_ty in
       let const =
         Parser.item xs
         @@ Parser.atom (fun s ->
           Int64.of_string_opt s
           |> Option.value_or_thunk ~default:(fun () ->
             Parser.parse_error [%message "couldn't parse number" ~s]))
       in
       Expr.Const { ty; const }
     | "cmp" ->
       let ty = Parser.item xs parse_ty in
       let op = Parser.item xs @@ Parser.atom parse_cmp_op in
       let v1 = Parser.item xs (parse_expr st) in
       let v2 = Parser.item xs (parse_expr st) in
       Expr.Cmp { ty; op; v1; v2 }
     | "add" -> parse_bin Bin_op.Add st xs
     | "sub" -> parse_bin Sub st xs
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
  | _ -> todo [%here]

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

let parse_lit s =
  Parser.atom (fun s' ->
    if [%equal: string] s s'
    then ()
    else Parser.parse_error [%message "expected literal" ~s ~got:s'])
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
       let ty = Expr.get_ty_exn expr in
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

module Decl = struct
  type 'v t =
    | Func of 'v Function.t
    | Extern of Extern.t
  [@@deriving sexp_of]
end

let parse_function sexp =
  let st = create_state () in
  let@ xs = Parser.list_ref sexp in
  let decl_name = Parser.item xs @@ Parser.string in
  match decl_name with
  | "define" ->
    let name, params =
      let@ x = Parser.item xs in
      let@ xs = Parser.list_ref x in
      let name = Parser.item xs parse_ident in
      let params = Parser.rest !xs (parse_value st) in
      name, params
    in
    let return = Parser.item xs parse_ty in
    let ty = { Named_function_ty.params; return } in
    let graph = parse_graph st !xs in
    Decl.Func
      { name
      ; ty
      ; graph
      ; unique_label = Intern_table.get_next_id st.label_intern
      ; unique_name = Intern_table.get_next_id st.name_intern
      }
  | "extern" ->
    let name, params =
      let@ x = Parser.item xs in
      let@ xs = Parser.list_ref x in
      let name = Parser.item xs parse_ident in
      let params = Parser.rest !xs parse_ty in
      name, params
    in
    let return = Parser.item xs parse_ty in
    let ty = { Function_ty.params; return } in
    Decl.Extern { name; ty }
  | _ -> raise_s [%message "didn't know how to parse declaration" decl_name]
;;

let parse_decls xs = Parser.rest xs parse_function
let empty_program : _ Program.t = { funcs = []; externs = [] }

let reverse_program (program : _ Program.t) =
  { program with funcs = List.rev program.funcs; externs = List.rev program.externs }
;;

let parse_program xs =
  let decls = parse_decls xs in
  List.fold_left decls ~init:empty_program ~f:(fun program decl ->
    match decl with
    | Decl.Func func -> { program with funcs = func :: program.funcs }
    | Extern extern -> { program with externs = extern :: program.externs })
  |> reverse_program
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
  print_s [%sexp (fns : _ Program.t)];
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
                 (expr (Bin (ty U64) (op Add) (v1 _) (v2 _))))))
              (exit (Ret ()))))))
          (exit first.0)))
        (ty
         ((params (((name first.0) (ty U64)) ((name second.1) (ty U64))))
          (return U64)))
        (unique_label 1) (unique_name 4))))
     (externs ())) |}]
;;
