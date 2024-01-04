open O
open Lir_instr
module Pretty = Sexp_lang.Pretty

let pretty_name = function
  | Name.Name s -> Pretty.Atom s
  | Name.GenName (s, i) -> Pretty.Atom (s ^ "." ^ string_of_int i)

let pretty_value (value : Value.t) = pretty_name value.name

let pretty_ty = function
  | Ty.U64 -> Pretty.(Atom "u64")
  | Ty.U1 -> Pretty.(Atom "u1")

let pretty_value_typed (value : Value.t) =
  Pretty.List [ pretty_name value.name; pretty_ty value.ty ]

let cmp_op_to_string = function CmpOp.Gt -> "gt"

let pretty_instr_op = function
  | InstrOp.Add { ty; v1; v2 } ->
      Pretty.(
        List [ Atom "add"; pretty_ty ty; pretty_value v1; pretty_value v2 ])
  | InstrOp.Sub { ty; v1; v2 } ->
      Pretty.(
        List [ Atom "sub"; pretty_ty ty; pretty_value v1; pretty_value v2 ])
  | InstrOp.Const { ty; const } ->
      Pretty.(
        List [ Atom "const"; pretty_ty ty; Atom (Int64.to_string_hum const) ])
  | InstrOp.Cmp { ty; op; v1; v2 } ->
      Pretty.(
        List
          [
            Atom "cmp";
            pretty_ty ty;
            Atom (cmp_op_to_string op);
            pretty_value v1;
            pretty_value v2;
          ])
  | InstrOp.Val { ty; v } -> Pretty.(List [ pretty_ty ty; pretty_value v ])

let pretty_block_call ({ label; args } : BlockCall.t) =
  Pretty.(
    List [ pretty_name label.name; Pretty.List (List.map ~f:pretty_value args) ])

let pretty_instr_control i =
  match i with
  | InstrControl.Jump v -> Pretty.(List [ Atom "jump"; pretty_block_call v ])
  | InstrControl.CondJump (v, j1, j2) ->
      Pretty.(
        List
          [
            Atom "cond_jump";
            pretty_value v;
            pretty_block_call j1;
            pretty_block_call j2;
          ])
  | InstrControl.Ret v ->
      Pretty.(
        List ([ Atom "ret" ] @ (Option.map ~f:pretty_value v |> Option.to_list)))

let pretty_assign i =
  let name, op = Instr.get_assign i in
  Pretty.(List [ Atom "set"; pretty_value name; pretty_instr_op op ])

let pretty_control op =
  let op = Instr.get_control op in
  pretty_instr_control op

let pretty_block (label : Label.t) (block : Block.t) =
  Pretty.(
    list
    @@ List.concat
         [
           [
             Atom "label";
             List
               ([ pretty_name label.name ]
               @ List.map ~f:pretty_value (Instr.get_block_args block.entry));
             Ann IndentLine;
           ];
           List.map ~f:pretty_assign block.body
           |> List_ext.end_with ~sep:(Ann Line);
           [ pretty_control block.exit ];
         ])

let pretty_graph (graph : Graph.t) =
  Pretty.(
    List.concat
      [
        [ pretty_block graph.entry (Map.find_exn graph.blocks graph.entry) ];
        Map.to_alist
          (Map.remove (Map.remove graph.blocks graph.entry) graph.exit)
        |> List.map ~f:(fun (label, block) -> pretty_block label block)
        |> List_ext.start_with ~sep:(Ann Line);
        (if not ([%equal: Label.t] graph.entry graph.exit) then
           [
             Ann Line;
             pretty_block graph.exit (Map.find_exn graph.blocks graph.exit);
           ]
         else []);
      ])

let pretty_function (fn : Function.t) =
  Pretty.(
    list
    @@ List.concat
         [
           [
             Atom "define";
             List
               ([ pretty_name fn.name ]
               @ List.map ~f:pretty_value_typed fn.params);
             pretty_ty fn.return_ty;
           ];
           [ Ann IndentLine ];
           pretty_graph fn.body;
         ])

let pretty_single fn = pretty_function fn |> Pretty.to_string

let pretty fns =
  fns
  |> List.map ~f:pretty_function
  |> List.map ~f:Pretty.to_string
  |> String.concat ~sep:"\n\n"
