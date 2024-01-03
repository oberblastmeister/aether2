open O

module Lir = struct
  include Lir_instr
end

exception Exn of Error.t

let run f = try Ok (f ()) with Exn e -> Error e
let elaborate_error e = raise (Exn e)

let collect_types (fn : Lir.Name.t Lir.Function.t') =
  let add_instr z (Lir.Instr.Some.T i) =
    match i with
    | Lir.Instr.Assign (v, _) ->
        A.map (A.Map.at v.name)
          ~f:(function
            | None -> Some (List1.singleton v.ty)
            | Some at -> Some List1.(v.ty |: at))
          z
    | _ -> z
  in
  let p = Lir.(Function.body @> Graph.blocks @> A.Map.each) in
  let tys_of_name =
    A.fold p fn ~init:Lir.Name.Map.empty ~f:(fun init block ->
        Lir.Block.fold_instrs_forward ~init ~f:add_instr block)
  in
  let tys_of_name_with_fn_params =
    List.fold_left ~init:tys_of_name
      ~f:(fun m param ->
        A.map (A.Map.at param.name) m ~f:(function
          | None -> Some (List1.singleton param.ty)
          | Some at -> Some List1.(param.ty |: at)))
      fn.params
  in
  let find_representative_ty name tys =
    if List1.all_equal [%equal: Lir.Ty.t] tys then List1.hd tys
    else
      elaborate_error
        (Error.t_of_sexp
           [%message
             "types weren't all equal"
               ~name:(name : Lir.Name.t)
               ~tys:(tys : Lir.Ty.t List1.t)])
  in
  Map.mapi tys_of_name_with_fn_params ~f:(fun ~key ~data ->
      find_representative_ty key data)

let elaborate_instr label ty_of_name instr =
  Lir.Instr.map_t'
    (fun name : Lir.Value.t ->
      {
        name;
        ty =
          Map.find ty_of_name name
          |> Option.value_or_thunk ~default:(fun () ->
                 elaborate_error
                   (Error.t_of_sexp
                      [%message
                        "name not defined"
                          ~name:(name : Lir.Name.t)
                          ~block_label:(label : Lir.Label.t)]));
      })
    instr

let elaborate_block label ty_of_name block =
  Lir.Block.map_forwards
    { f = (fun instr -> elaborate_instr label ty_of_name instr) }
    block

let elaborate_function (fn : Lir.Name.t Lir.Function.t') =
  let ty_of_name = collect_types fn in
  let p = Lir.(Function.body @> Graph.blocks @> A.Map.eachi) in
  A.mapi p fn ~f:(fun label block ->
      elaborate_block (A.Index.hd label) ty_of_name block)

let elaborate_single fn = run (fun () -> elaborate_function fn)
let elaborate fns = run (fun () -> List.map ~f:elaborate_function fns)

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
  |}
  in
  let fns = Lir_parse.parse s |> Or_error.ok_exn in
  let fns = fns |> elaborate |> Or_error.ok_exn in
  (* printf "sexp:\n";
  print_s @@ [%sexp_of: Lir.Function.t list] fns; *)
  printf "pretty:\n";
  print_endline @@ Lir_pretty.pretty fns;
  ();
  [%expect {|
    pretty:
    (define (testing (first u64) (second u64)) u64
      (label (first arg)
        (set x (add u64 first second))
        (ret))
      (label (second arg)
        (set x (add u64 first second))
        (ret))
      (label (third arg)
        (set x (add u64 first second))
        (ret))) |}]
