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
          let op = Parser.item xs parse_var in
          Lir.InstrOp.Val (ty, op)
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
          Lir.InstrOp.Const (ty, const)
      | "cmp" ->
          let ty = Parser.item xs parse_ty in
          let cmp_op = Parser.item xs @@ Parser.atom parse_cmp_op in
          let v1 = Parser.item xs parse_var in
          let v2 = Parser.item xs parse_var in
          Lir.InstrOp.Cmp (ty, cmp_op, v1, v2)
      | "add" ->
          let ty = Parser.item xs parse_ty in
          let op1 = Parser.item xs parse_var in
          let op2 = Parser.item xs parse_var in
          Lir.InstrOp.Add (ty, op1, op2)
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
      ({ label; args } : Lir.Name.t Lir.BlockCall.t'))

module Some = struct
  type t = T : (Lir.Name.t, 'c) Lir.Instr.t' -> t
end

let parse_instr =
  let instr_o name instr_op : Some.t =
    Some.T (Lir.Instr.Assign (name, instr_op))
  in
  Parser.list_ref (fun xs ->
      let instr_name = Parser.item xs parse_ident in
      match instr_name with
      | "set" ->
          let name = Parser.item xs parse_value in
          let instr = Parser.item xs parse_op_instr in
          instr_o name instr
      | _ -> todo ())

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
      (* ( label,
        ({ entry = Lir.Instr.BlockArgs args; body = instrs; exit = last_instr }
          : Lir.WithName.Block.t) *)
          todo ()
          )

(* let parse_graph xs =
   let blocks = Parser.rest xs parse_block in
   match blocks with
   | (label, block) :: _ ->
     *)
