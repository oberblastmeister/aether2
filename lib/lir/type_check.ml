open O
open Ast

type context =
  { temps : Ty.t Name.Table.t
  ; func_tys : Function_ty.t String.Map.t
      (* The current graph for the function. *)
      (* May be a garbage value at initialization *)
  ; graph : Value.t Graph.t
  }
[@@deriving sexp_of]

type error = [ `LirTypeCheckError of Sexp.t ] [@@deriving sexp_of]

exception Exn of Sexp.t

let error e = raise_notrace (Exn e)

type more_errors =
  [ `More
  | error
  ]

let testing : [ `Another ] = `Another

let handle f =
  match f () with
  | exception Exn e -> Error (`LirTypeCheckError e)
  | x -> Ok x
;;

let garbage_graph =
  let garbage_label = Label.create "garbage_label" (Label.Id.of_int (-1)) in
  Cfg.Graph.of_alist ~entry:garbage_label ~exit:garbage_label []
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
  let temps = Name.Table.create () in
  { temps; func_tys; graph = garbage_graph }
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

let check_expr cx (expr : Value.t Expr.t) =
  match expr with
  | Bin { ty; v1; v2; _ } ->
    check_ty_equal ty v1.ty;
    check_ty_equal ty v2.ty;
    ()
  | Const { ty; const } ->
    (match ty with
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
  | Call { ty; name; args } ->
    let func_ty =
      match Map.find cx.func_tys name with
      | Some func_ty -> func_ty
      | None -> error [%message "unknown function" (name : string)]
    in
    check_ty_equal ty func_ty.return;
    check_args func_ty.params args;
    ()
  | _ -> todo [%here]
;;

let check_instr cx (instr : _ Instr.t) =
  match instr with
  | Assign { dst; expr } ->
    let ty = Expr.get_ty expr in
    check_ty_equal dst.ty ty;
    check_expr cx expr;
    ()
  | _ -> todo [%here]
;;

let check_block_call cx (j : _ Block_call.t) =
  let block =
    match Cfg.Graph.find j.label cx.graph with
    | Some block -> block
    | None -> error [%message "could not find block with label" (j.label : Label.t)]
  in
  check_args (List.map ~f:(fun v -> v.ty) block.entry) j.args;
  ()
;;

let check_control_instr cx (instr : _ Control_instr.t) =
  Control_instr.iter_block_calls instr ~f:(check_block_call cx);
  (* TODO: check that return can't return for void *)
  match instr with
  | Ret _ -> ()
  | _ -> ()
;;

let check_some_instr cx instr =
  match Some_instr.to_variant instr with
  | Block_args _ -> ()
  | Instr i -> check_instr cx i
  | Control i -> check_control_instr cx i
;;

let check_func cx (func : _ Function.t) =
  Entity.Map.clear cx.temps;
  let cx = { cx with graph = func.graph } in
  Function.iter_instrs_forward func ~f:(check_some_instr cx);
  ()
;;

let check_program program =
  let cx = create_context program in
  List.iter program.funcs ~f:(check_func cx);
  ()
;;

let run program = (handle (fun () -> check_program program) :> (_, [> error ]) result)
