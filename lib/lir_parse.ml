open O

module Lir = struct
  include Lir_instr
end

module Parser = Sexp_lang.Parser

let parse_ty =
  Parser.atom (function
    | "u64" -> Lir.Ty.U64
    | "u1" -> Lir.Ty.U1
    | _ -> Parser.parse_error [%message "unknown type"])

let parse_var = Parser.atom (fun s -> Lir.Name.of_string s)

let parse_value =
  Parser.list_ref (fun xs ->
      let name = Parser.item xs parse_var in
      let ty = Parser.item xs parse_ty in
      ({ name; ty } : Lir.Value.t))

let parse_ident = Parser.atom Fn.id

let parse_cmp_op = function
  | "gt" -> Lir.CmpOp.Gt
  | s -> Parser.parse_error [%message "unknown cmp op" ~op:s]

let parse_op_instr =
  Parser.list_ref (fun xs ->
      let name = Parser.item xs parse_ident in
      match name with
      | "val" ->
          let ty = Parser.item xs parse_ty in
          let v = Parser.item xs parse_var in
          Lir.InstrOp.Val { ty; v }
      | "const" ->
          let ty = Parser.item xs parse_ty in
          let const =
            Parser.item xs
            @@ Parser.atom (fun s ->
                   Int64.of_string_opt s
                   |> Option.value_or_thunk ~default:(fun () ->
                          Parser.parse_error
                            [%message "couldn't parse number" ~s]))
          in
          Lir.InstrOp.Const { ty; const }
      | "cmp" ->
          let ty = Parser.item xs parse_ty in
          let op = Parser.item xs @@ Parser.atom parse_cmp_op in
          let v1 = Parser.item xs parse_var in
          let v2 = Parser.item xs parse_var in
          Lir.InstrOp.Cmp { ty; op; v1; v2 }
      | "add" ->
          let ty = Parser.item xs parse_ty in
          let v1 = Parser.item xs parse_var in
          let v2 = Parser.item xs parse_var in
          Lir.InstrOp.Add { ty; v1; v2 }
      | _ -> Parser.parse_error [%message "unknown op" ~name])

let parse_label = Parser.atom Lir.Label.of_string

let parse_lit s =
  Parser.atom (fun s' ->
      if [%equal: string] s s' then ()
      else Parser.parse_error [%message "expected literal" ~s ~got:s'])

let parse_block_call =
  Parser.list_ref (fun xs ->
      let label = Parser.item xs parse_label in
      let args = Parser.rest !xs parse_var in
      ({ label; args } : _ Lir.BlockCall.t'))

module Some = struct
  type t = T : (Lir.Name.t, 'c) Lir.Instr.t' -> t
end

let parse_instr =
  let instr_o name instr_op : Some.t =
    Some.T (Lir.Instr.Assign (name, instr_op))
  in
  let instr_c instr_control : Some.t =
    Some.T (Lir.Instr.Control instr_control)
  in
  Parser.list_ref (fun xs ->
      let instr_name = Parser.item xs parse_ident in
      match instr_name with
      | "set" ->
          let name = Parser.item xs parse_var in
          let instr = Parser.item xs parse_op_instr in
          let ty = Lir.InstrOp.get_ty instr in
          instr_o { name; ty } instr
      | "ret" ->
          let v = Parser.optional_item !xs parse_var in
          instr_c (Lir.InstrControl.Ret v)
      | _ ->
          Parser.parse_error [%message "unknown instruction" ~name:instr_name])

let parse_block =
  Parser.list_ref (fun xs ->
      Parser.item xs @@ parse_lit "label";
      let label, args =
        Parser.item xs
        @@ Parser.list_ref (fun xs ->
               let label = Parser.item xs parse_label in
               let args = Parser.rest !xs parse_value in
               (label, args))
      in
      let instrs = Parser.rest !xs parse_instr in
      let instrs, last_instr =
        List_ext.unsnoc_list instrs
        |> Option.value_or_thunk ~default:(fun () ->
               Parser.parse_error [%message "cannot have empty block"])
      in
      let instrs =
        List.map instrs ~f:(fun (Some.T i) ->
            match i with
            | Lir.Instr.Assign (name, instr_op) ->
                Lir.Instr.Assign (name, instr_op)
            | _ ->
                Parser.parse_error
                  [%message
                    "instructions before the end must be op instrs"
                      ~got:(Lir.Instr.sexp_of_t' Lir.Name.sexp_of_t i : Sexp.t)])
      in
      let last_instr =
        let (Some.T i) = last_instr in
        match i with
        | Lir.Instr.Control i -> Lir.Instr.Control i
        | _ ->
            Parser.parse_error
              [%message
                "last instruction must be control instruction"
                  ~got:(Lir.Instr.sexp_of_t' Lir.Name.sexp_of_t i : Sexp.t)]
      in
      ( label,
        ({ entry = Lir.Instr.BlockArgs args; body = instrs; exit = last_instr }
          : _ Lir.Block.t') ))

let parse_graph xs =
  let blocks = Parser.rest xs parse_block in
  match blocks with
  | (label, _) :: _ ->
      ({
         entry = label;
         blocks = Lir.Label.Map.of_alist_exn blocks;
         exit = List.last_exn blocks |> fst;
       }
        : _ Lir.Graph.t')
  | _ -> Parser.parse_error [%message "graph must have at least one block"]

let parse_function =
  Parser.list_ref (fun xs ->
      Parser.item xs @@ parse_lit "define";
      let name, params =
        Parser.item xs
        @@ Parser.list_ref (fun xs ->
               let name = Parser.item xs parse_var in
               let params = Parser.rest !xs parse_value in
               (name, params))
      in
      let return_ty = Parser.item xs parse_ty in
      let body = parse_graph !xs in
      ({ name; params; return_ty; body } : _ Lir.Function.t'))

let parse s =
  let open Result.Let_syntax in
  let%bind sexp = Sexp_lang.Syntax.parse s in
  let%bind functions =
    Sexp_lang.Parser.run (fun () -> List.map ~f:parse_function sexp)
    |> Result.map_error ~f:Parser.Error.to_error
  in
  return functions

let%expect_test _ =
  let s =
    {|
(define (testing (first u64) (second u64)) u64
    (label (first (arg u64))
        (set x (add u64 first second))
        (ret)))
  |}
  in
  let fns = parse s |> Or_error.ok_exn in
  print_s [%sexp (fns : _ Lir.Function.t' list)];
  ();
  [%expect {|
    (((name (Name testing))
      (params (((name (Name first)) (ty U64)) ((name (Name second)) (ty U64))))
      (body
       ((entry ((name (Name first))))
        (blocks
         ((((name (Name first)))
           ((entry (BlockArgs (((name (Name arg)) (ty U64)))))
            (body
             ((Assign ((name (Name x)) (ty U64)) (Add (ty U64) (v1 _) (v2 _)))))
            (exit (Control (Ret ())))))))
        (exit ((name (Name first))))))
      (return_ty U64))) |}]
