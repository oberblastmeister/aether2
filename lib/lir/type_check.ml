open O
open Ast

(* we don't need to put temps in here because they are already handled by the elaboration phase *)
type context = { decls : Value.t Decl.t String.Map.t } [@@deriving sexp_of]

exception Exn of Error.t

let error e = raise_notrace (Exn (Error.t_of_sexp e))

let with_tag tag f =
  try f () with
  | Exn e -> raise_notrace (Exn (Error.tag_s e ~tag))
;;

let handle f =
  match f () with
  | exception Exn e -> Error e
  | x -> Ok x
;;

let create_context (modul : _ Module.t) =
  let decls =
    match Module.get_decl_map modul with
    | Error name -> error [%message "duplicate name " ~name]
    | Ok x -> x
  in
  (*
     let func_tys = String.Map.empty in
     let func_tys =
     List.fold_left program.funcs ~init:func_tys ~f:(fun funcs func ->
     match Map.add funcs ~key:func.name ~data:(Named_function_ty.to_anon func.ty) with
     | `Ok funcs -> funcs
     | `Duplicate ->
     error [%message "duplicate function name" ~name:(func.name : string)])
     in
     let func_tys =
     List.fold_left program.externs ~init:func_tys ~f:(fun funcs extern ->
     match Map.add funcs ~key:extern.name ~data:extern.ty with
     | `Ok funcs -> funcs
     | `Duplicate ->
     error [%message "duplicate function name" ~name:(extern.name : string)])
     in *)
  { decls }
;;

let check_ty_equal ty1 ty2 =
  if not (Ty.equal ty1 ty2)
  then error [%message "type mismatch" (ty1 : Ty.t) (ty2 : Ty.t)]
;;

let check_args params (args : Value.t Expr.t list) =
  let args, excess_args = List.zip_with_remainder params args in
  (match excess_args with
   | None -> ()
   | Some (First needed_args) ->
     error [%message "not enough arguments" (needed_args : Ty.t list)]
   | Some (Second excess_args) ->
     error [%message "too many arguments" (excess_args : Value.t Expr.t list)]);
  List.iter args ~f:(fun (ty, arg) -> check_ty_equal ty (Expr.get_ty arg));
  ()
;;

let find_func_ty cx name =
  match Map.find cx.decls name with
  | None -> error [%message "could not find name" (name : string)]
  | Some (Func func) -> func.ty
  | Some (Func_def func_def) -> func_def.ty
  | Some decl -> error [%message "decl was not a func" (name : string) (decl : _ Decl.t)]
;;

let find_global cx name =
  match Map.find cx.decls name with
  | None -> error [%message "could not find name" (name : string)]
  | Some (Global global) -> global
  | Some decl ->
    error [%message "decl was not a global" (name : string) (decl : _ Decl.t)]
;;

let check_call cx is_void (ty : Ty.t) Call.{ name; args } =
  let func_ty = find_func_ty cx name in
  let func_ty = Named_function_ty.to_anon func_ty in
  (match is_void, ty with
   | false, Void ->
     error [%message "cannot assign the result of function that returns void"]
   | _ -> ());
  check_ty_equal ty func_ty.return;
  check_args func_ty.params args;
  ()
;;

let check_ty_bin_op ty =
  match ty with
  | Ty.I64 -> ()
  | _ -> error [%message "type not supported" (ty : Ty.t)]
;;

let check_ty_cmp_op ty op =
  match ty with
  | Ty.I64 -> ()
  | _ -> error [%message "type not supported" (ty : Ty.t) (op : Cmp_op.t)]
;;

let rec check_expr cx (expr : Value.t Expr.t) =
  match expr with
  | Bin { ty = Ptr; v1; v2; op = Add | Sub } ->
    check_expr cx v1;
    check_expr cx v2;
    let ty1 = Expr.get_ty v1 in
    let ty2 = Expr.get_ty v2 in
    (match ty1, ty2 with
     | Ptr, I64 | I64, Ptr -> ()
     | _ -> error [%message "invalid pointer operations" (ty1 : Ty.t) (ty2 : Ty.t)]);
    ()
  | Bin { ty = Ptr; op = Mul; _ } -> raise_s [%message "invalid pointer operations"]
  | Bin { ty; v1; v2; op = _ } ->
    check_ty_bin_op ty;
    check_ty_equal ty (Expr.get_ty v1);
    check_ty_equal ty (Expr.get_ty v2);
    check_expr cx v1;
    check_expr cx v2;
    ()
  | Const { ty; const } ->
    (match ty with
     | I1 ->
       assert_s
         Z.(of_int (-1) <= const && const <= of_int 1)
         [%message "I1 constant out of range" (const : Z.t)]
     | I64 ->
       assert_s
         Z.(
           of_int64 Int64.min_value <= const
           && const <= shift_left (of_int 1) 64 - of_int 1)
         [%message "I64 constant out of range" (const : Z.t)]
     | Void | Ptr -> error [%message "cannot use const with ty" (const : Z.t) (ty : Ty.t)])
  | Cmp { ty; signed = _; v1; v2; op } ->
    check_ty_cmp_op ty op;
    check_ty_equal ty (Expr.get_ty v1);
    check_ty_equal ty (Expr.get_ty v2);
    check_expr cx v1;
    check_expr cx v2;
    ()
  | Val _v -> ()
;;

let check_impure_expr cx (expr : _ Impure_expr.t) =
  match expr with
  | Alloca _ -> ()
  | Call { ty; call } ->
    check_call cx false ty call;
    ()
  | Global { name } ->
    let _global = find_global cx name in
    ()
  | Idiv { ty; v1; v2 } | Udiv { ty; v1; v2 } ->
    check_ty_bin_op ty;
    check_ty_equal ty (Expr.get_ty v1);
    check_ty_equal ty (Expr.get_ty v2);
    check_expr cx v1;
    check_expr cx v2
  | Impure_expr.Load { ty = _; ptr } ->
    check_ty_equal Ptr (Expr.get_ty ptr);
    check_expr cx ptr;
    ()
;;

let check_instr cx (instr : _ Instr.t) =
  let@ () = with_tag [%message (instr : Value.t Instr.t)] in
  match instr with
  | ImpureAssign { dst; expr } ->
    let ty = Impure_expr.get_ty expr in
    check_ty_equal dst.ty ty;
    check_impure_expr cx expr;
    ()
  | Assign { dst; expr } ->
    let ty = Expr.get_ty expr in
    check_ty_equal dst.ty ty;
    check_expr cx expr;
    ()
  | VoidCall call -> check_call cx true Void call
  | Instr.Store { ty; ptr; expr } ->
    check_ty_equal ty (Expr.get_ty expr);
    check_ty_equal Ptr (Expr.get_ty ptr);
    check_expr cx ptr;
    check_expr cx expr;
    ()
;;

let check_block_call _cx (func : _ Function.t) (j : _ Block_call.t) =
  let block =
    match Cfg.Graph.find j.label func.graph with
    | Some block -> block
    | None -> error [%message "could not find block with label" (j.label : Label.t)]
  in
  check_args (List.map ~f:(fun v -> v.ty) block.entry) j.args;
  ()
;;

let check_control_instr cx (func : _ Function.t) (instr : _ Control_instr.t) =
  let@ () = with_tag [%message (instr : Value.t Control_instr.t)] in
  Control_instr.iter_block_calls instr ~f:(fun j ->
    if Label.equal j.label (Cfg.Graph.entry func.graph)
    then error [%message "cannot jump to start label" ~label:(j.label : Label.t)];
    check_block_call cx func j)
;;

let check_block cx (func : _ Function.t) label (block : _ Block.t) =
  let@ () = with_tag [%message (label : Label.t)] in
  let graph = func.graph in
  List.iter block.body ~f:(check_instr cx);
  if Label.equal (Cfg.Graph.exit graph) label
  then (
    match block.exit with
    | Ret v ->
      (match v, func.ty.return with
       | None, Void -> ()
       | None, ty -> error [%message "must return" (ty : Ty.t)]
       | Some v, Void -> error [%message "cannot return anything" (v : Value.t Expr.t)]
       | Some _, _ty -> ())
    | instr ->
      error
        [%message "the last block must have a return" (instr : Value.t Control_instr.t)]);
  check_control_instr cx func block.exit;
  ()
;;

let check_func cx (func : _ Function.t) =
  let@ () = with_tag [%message (func.name : string)] in
  Cfg.Graph.iteri func.graph ~f:(fun (label, block) ->
    check_block cx func label block;
    if Label.equal label (Cfg.Graph.exit func.graph)
    then (
      (match block.exit with
       | Ret _ -> ()
       | instr ->
         error
           [%message
             "exit block must have a return"
               (instr : Value.t Control_instr.t)
               (label : Label.t)]);
      ())
    else (
      (match block.exit with
       | Ret _ ->
         error [%message "non exit block must not end with a return" (label : Label.t)]
       | _ -> ());
      ()));
  ()
;;

let check_decl cx decl =
  match decl with
  | Decl.Func func -> check_func cx func
  | Func_def _ | Global _ -> ()
;;

let check_module modul =
  let cx = create_context modul in
  List.iter modul.decls ~f:(check_decl cx);
  ()
;;

let run program = handle (fun () -> check_module program)
