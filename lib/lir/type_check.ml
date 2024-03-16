open O
open Ast

(* we don't need to put temps in here because they are already handled by the elaboration phase *)
type context = { func_tys : Function_ty.t String.Map.t } [@@deriving sexp_of]

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

let create_context (program : _ Program.t) =
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
  in
  { func_tys }
;;

let check_ty_equal ty1 ty2 =
  if not (Ty.equal ty1 ty2)
  then error [%message "type mismatch" (ty1 : Ty.t) (ty2 : Ty.t)]
;;

let check_args params args =
  let args, excess_args = List.zip_with_remainder params args in
  (match excess_args with
   | None -> ()
   | Some (First needed_args) ->
     error [%message "not enough arguments" (needed_args : Ty.t list)]
   | Some (Second excess_args) ->
     error [%message "too many arguments" (excess_args : Value.t list)]);
  List.iter args ~f:(fun (ty, arg) -> check_ty_equal ty arg.ty);
  ()
;;

let check_call cx is_void (ty : Ty.t) Call.{ name; args } =
  let func_ty =
    match Map.find cx.func_tys name with
    | Some func_ty -> func_ty
    | None -> error [%message "unknown function" (name : string)]
  in
  (match is_void, ty with
   | false, Void ->
     error [%message "cannot assign the result of function that returns void"]
   | _ -> ());
  check_ty_equal ty func_ty.return;
  check_args func_ty.params args;
  ()
;;

let check_expr cx (expr : Value.t Expr.t) =
  match expr with
  | Bin { ty; v1; v2; _ } ->
    check_ty_equal ty v1.ty;
    check_ty_equal ty v2.ty;
    ()
  | Const { ty; const } ->
    (match ty with
     | Void -> error [%message "cannot use const with unit" (const : int64)]
     | U1 when Int64.(const <= 1L) -> ()
     | U1 -> error [%message "U1 constant out of range" (const : int64)]
     | U64 -> ())
  | Cmp { ty; v1; v2; _ } ->
    check_ty_equal ty v1.ty;
    check_ty_equal ty v2.ty;
    ()
  | Val { ty; v } ->
    check_ty_equal ty v.ty;
    ()
  | Alloca _ -> ()
  | Call { ty; call } ->
    check_call cx false ty call;
    ()
  | _ -> todo [%here]
;;

let check_instr cx (instr : _ Instr.t) =
  let@ () = with_tag [%message (instr : Value.t Instr.t)] in
  match instr with
  | Assign { dst; expr } ->
    let ty = Expr.get_ty expr in
    check_ty_equal dst.ty ty;
    check_expr cx expr;
    ()
  | VoidCall call -> check_call cx true Void call
  | _ -> todo [%here]
;;

let check_block_call cx (func : _ Function.t) (j : _ Block_call.t) =
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
       | Some v, Void -> error [%message "cannot return anything" (v : Value.t)]
       | Some _, _ty -> ())
    | instr ->
      error
        [%message "the last block must have a return" (instr : Value.t Control_instr.t)]);
  check_control_instr cx func block.exit;
  ()
;;

let check_func cx (func : _ Function.t) =
  let@ () = with_tag [%message (func.name : string)] in
  Cfg.Graph.iteri func.graph ~f:(fun (label, block) -> check_block cx func label block);
  ()
;;

let check_program program =
  let cx = create_context program in
  List.iter program.funcs ~f:(check_func cx);
  ()
;;

let run program = handle (fun () -> check_program program)
