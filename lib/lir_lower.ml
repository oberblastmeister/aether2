open O
module Lir = Lir_instr

module Value = struct
  type t =
    | V of Lir.Value.t
    | I of t Lir.InstrOp.t'
  [@@deriving sexp_of]
end

module Instr = struct
  include Lir.Instr

  type 'c t = (Value.t, 'c) Lir.Instr.t'
end

module Block = struct
  include Lir.Block

  type t = Value.t t' [@@deriving sexp_of]
end

module Graph = struct
  include Lir.Graph

  type t = Value.t t' [@@deriving sexp_of]
end

module Function = struct
  include Lir.Function

  type t = Value.t t' [@@deriving sexp_of]
end

module Program = struct
  include Lir.Program

  type t = Value.t t' [@@deriving sexp_of]
end

let rec pretty_value = function
  | Value.V v -> Lir_pretty.pretty_value v
  | Value.I i -> Lir_pretty.pretty_instr_op { pretty_value } i
;;

let pretty = Lir_pretty.pretty' { pretty_value }

(* TODO: this is wrong, use the cranelift coloring scheme *)
(* https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/codegen/src/machinst/lower.rs#L693 *)
(* the uses should be deep also, because we might want to duplicate compare instructions, which will duplicate their transitive dependencies *)
let get_instr_uses (fn : Lir.Function.t) =
  let instr_uses = Lir.Value.Hashtbl.create () in
  let fold = F.Fold.(Lir.Function.instrs_forward_fold @> Lir.Instr.uses_fold) in
  F.Fold.iter fold fn ~f:(fun (use : Lir.Value.t) ->
    Hashtbl.update instr_uses use ~f:(Option.value_map ~default:1 ~f:succ));
  instr_uses
;;

let take_inlinable_instrs instr_uses (fn : Lir.Function.t) =
  let inlinable_instrs = Lir.Value.Hashtbl.create () in
  let rec go_block_body = function
    | instr :: instrs ->
      (match instr, instrs with
       | Lir.Instr.Assign (_, Lir.InstrOp.Store _), _ -> instr :: go_block_body instrs
       | Lir.Instr.Assign (dest, (Lir.InstrOp.Load _ as op)), instr' :: _
         when Hashtbl.find instr_uses dest |> [%equal: int option] (Some 1)
              && Lir.Instr.uses instr'
                 |> List.find ~f:([%equal: Lir.Value.t] dest)
                 |> Option.is_some ->
         Hashtbl.add_exn ~key:dest ~data:op inlinable_instrs;
         go_block_body instrs
       | Lir.Instr.Assign (dest, op), _
         when Hashtbl.find instr_uses dest |> [%equal: int option] (Some 1) ->
         Hashtbl.add_exn ~key:dest ~data:op inlinable_instrs;
         go_block_body instrs
       | _ -> instr :: go_block_body instrs)
    | [] -> []
  in
  let fn =
    (Function.map_blocks & FC.Map.map) fn ~f:(fun block ->
      let body = go_block_body block.body in
      { block with body })
  in
  inlinable_instrs, fn
;;

type lower_state = { inlinable_instrs : Lir.InstrOp.t Lir.Value.Hashtbl.t }

let rec lower_value st value =
  match Hashtbl.find st.inlinable_instrs value with
  | None -> Value.V value
  | Some op -> Value.I (Lir.InstrOp.map_t' (lower_value st) op)
;;

let lower_instr st instr =
  let instr = Instr.map_uses instr ~f:(lower_value st) in
  instr
;;

let lower_function (fn : Lir.Function.t) : Function.t =
  let instr_uses = get_instr_uses fn in
  let inlinable_instrs, fn = take_inlinable_instrs instr_uses fn in
  let st = { inlinable_instrs } in
  let fn =
    (Function.map_blocks & FC.Map.map) fn ~f:(fun block ->
      Block.map_instrs_forwards
        { f =
            (fun instr ->
              let instr = lower_instr st instr in
              instr)
        }
        block)
  in
  fn
;;

let lower_program (prog : Lir.Program.t) : Program.t =
  let prog = (Program.map_functions & List.map) prog ~f:lower_function in
  prog
;;
