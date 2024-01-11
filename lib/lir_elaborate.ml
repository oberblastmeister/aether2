open O
open Lir_instr

exception Exn of Error.t

let run f =
  try Ok (f ()) with
  | Exn e -> Error e
;;

let elaborate_error e = raise (Exn e)

let collect_types (fn : Name.t Function.t) =
  let add_instr z (SomeInstr.T i) =
    match i with
    | InstrGeneric.Instr (Instr.Assign { dst; _ }) ->
      Map.update z dst.name ~f:(function
        | None -> List1.singleton dst.ty
        | Some at -> List1.(dst.ty |: at))
    | _ -> z
  in
  let tys_of_name =
    F.Fold.reduce
      (F.Core.Map.fold @> Block.instrs_forward_fold)
      (F.Reduce.T (add_instr, Name.Map.empty, Fn.id))
      fn.graph.blocks
  in
  let tys_of_name_with_fn_params =
    List.fold_left
      ~init:tys_of_name
      ~f:(fun m param ->
        Map.update m param.name ~f:(function
          | None -> List1.singleton param.ty
          | Some at -> List1.(param.ty |: at)))
      fn.params
  in
  let find_representative_ty name tys =
    if List1.all_equal [%equal: Ty.t] tys
    then List1.hd tys
    else
      elaborate_error
        (Error.t_of_sexp
           [%message
             "types weren't all equal" ~name:(name : Name.t) ~tys:(tys : Ty.t List1.t)])
  in
  Map.mapi tys_of_name_with_fn_params ~f:(fun ~key ~data ->
    find_representative_ty key data)
;;

let elaborate_instr label ty_of_name instr =
  InstrGeneric.map
    ~f:(fun name : Value.t ->
      { name
      ; ty =
          Map.find ty_of_name name
          |> Option.value_or_thunk ~default:(fun () ->
            elaborate_error
              (Error.t_of_sexp
                 [%message
                   "name not defined" ~name:(name : Name.t) ~block_label:(label : Label.t)]))
      })
    instr
;;

let elaborate_block label ty_of_name block =
  Block.map_instrs_forwards
    { f = (fun instr -> elaborate_instr label ty_of_name instr) }
    block
;;

let elaborate_function (fn : Name.t Function.t) : Value.t Function.t =
  let ty_of_name = collect_types fn in
  let map =
    (fun (x : _ Function.t) ~f -> { x with graph = f x.graph })
    & (fun (x : _ Graph.t) ~f -> { x with blocks = f x.blocks })
    & F.Core.Map.mapi
  in
  let res = map fn ~f:(fun (label, block) -> elaborate_block label ty_of_name block) in
  res
;;

let elaborate_single fn = run (fun () -> elaborate_function fn)

let elaborate_program (program : Name.t Program.t) =
  let new_functions = List.map ~f:elaborate_function program.functions in
  { program with functions = new_functions }
;;

let elaborate (program : Name.t Program.t) : Value.t Program.t Or_error.t =
  run (fun () -> elaborate_program program)
;;

let%expect_test _ =
  let s =
    {|
(define (          testing
(first u64)
(second u64))
u64
(label (first (arg u64)) (set x (add u64 first second)) (ret))
(label (second (arg u64)) (set x (add u64 first second)) (ret))
(label (third (arg u64)) (set x (add u64 first second)) (ret))
)

(define (another) u64 (label (start) (ret)))
  |}
  in
  let program = Lir_parse.parse s |> Or_error.ok_exn in
  let program = program |> elaborate |> Or_error.ok_exn in
  (* printf "sexp:\n";
     print_s @@ [%sexp_of: Function.t list] fns; *)
  printf "pretty:\n";
  print_endline @@ Lir_pretty.pretty program;
  ();
  [%expect
    {|
    pretty:
    (define (testing (first u64) (second u64)) u64
      (label (first (arg u64))
        (set x (add u64 first second))
        (ret))
      (label (second (arg u64))
        (set x (add u64 first second))
        (ret))
      (label (third (arg u64))
        (set x (add u64 first second))
        (ret)))

    (define (another) u64
      (label (start)
        (ret))) |}]
;;
