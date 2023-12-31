open O
module Control = Cfg.Control

module Ty = struct
  type t = U1 | U64 [@@deriving equal, compare, sexp, hash, accessors]
end

module Value = struct
  type t = { name : string; ty : Ty.t }
  [@@deriving equal, compare, sexp, accessors]
end

module CmpOp = struct
  type t = Gt [@@deriving equal, compare, sexp, accessors]
end

module InstrOp = struct
  type 'v t' =
    | Add of (Ty.t * 'v * 'v)
    | Sub of (Ty.t * 'v * 'v)
    | Const of (Ty.t * Int64.t)
    | Cmp of (Ty.t * CmpOp.t * 'v * 'v)
    | Val of (Ty.t * 'v)
  [@@deriving equal, compare, sexp, accessors]

  type t = Value.t t' [@@deriving equal, compare, sexp]
end

module BlockCall = struct
  type 'v t' = { label : Cfg.Label.t; args : 'v list }
  [@@deriving equal, compare, sexp, accessors]

  type t = Value.t t' [@@deriving equal, compare, sexp]
end

module InstrControl = struct
  type 'v t' =
    | Jump of 'v BlockCall.t'
    | CondJump of ('v * 'v BlockCall.t' * 'v BlockCall.t')
    | Ret of 'v
  [@@deriving equal, compare, sexp, accessors]

  type t = Value.t t' [@@deriving equal, compare, sexp]
end

module Instr = struct
  type ('v, 'c) t' =
    | BlockArgs : Value.t list -> ('v, Control.e) t'
    | Assign : ('v * 'v InstrOp.t') -> ('v, Control.o) t'
    | Control : 'v InstrControl.t' -> ('v, Control.c) t'

  type 'c t = (Value.t, 'c) t'

  let sexp_of_t (type c) (i : c t) =
    match i with
    | Control c -> [%sexp "Control", (c : InstrControl.t)]
    | BlockArgs vs -> [%sexp "BlockArgs", (vs : Value.t list)]
    | Assign (v, op) ->
        [%sexp "Assign", (v : Value.t), (op : Value.t InstrOp.t')]
end

let%expect_test _ =
  let v : Value.t = { name = "x"; ty = Ty.U64 } in
  [%sexp_of: Instr.t] (Assign (v, Add (Ty.U64, v, v)))
  |> Sexp.to_string_hum |> printf "%s";
  [%expect
    {|
    (Assign ((name x) (ty U64))
     (Add (U64 ((name x) (ty U64)) ((name x) (ty U64))))) |}]

let%test _ = 1234 = 1234
