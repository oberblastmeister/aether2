open O
open Instr_types

module Ty = struct
  type t = U1 | U64 [@@deriving equal, compare, sexp, hash, accessors]
end

module Name = Name
module Label = Label

module Value = struct
  type t = {
    name : Name.t;
    ty : Ty.t; [@equal.ignore] [@compare.ignore] [@hash.ignore]
  }
  [@@deriving equal, compare, hash, sexp, accessors]
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
  type 'v t' = { label : Label.t; args : 'v list }
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

  let jumps i =
    match i with
    | Jump j -> [ j.label ]
    | CondJump (_, j1, j2) -> [ j1.label; j2.label ]
    | Ret _ -> []
end

module Instr = struct
  module T = struct
    type ('v, 'c) t' =
      | BlockArgs : Value.t list -> ('v, Control.e) t'
      | Assign : (Value.t * 'v InstrOp.t') -> ('v, Control.o) t'
      | Control : 'v InstrControl.t' -> ('v, Control.c) t'

    type 'c t = (Value.t, 'c) t'

    let sexp_of_t' (type c v) (f : v -> Sexp.t) (i : (v, c) t') =
      match i with
      | Control c -> [%sexp "Control", (InstrControl.sexp_of_t' f c : Sexp.t)]
      | BlockArgs vs -> [%sexp "BlockArgs", (vs : Value.t list)]
      | Assign (v, op) ->
          [%sexp "Assign", (v : Value.t), (InstrOp.sexp_of_t' f op : Sexp.t)]

    let sexp_of_t instr = sexp_of_t' Value.sexp_of_t instr
  end

  include T
  include Higher_kinded.Make (T)

  let type_equal (type c d) (i1 : c t) (i2 : d t) : (c, d) Type_equal.t Option.t
      =
    match (i1, i2) with
    | Control c1, Control c2 when [%equal: InstrControl.t] c1 c2 ->
        Some Type_equal.refl
    | BlockArgs vs1, BlockArgs vs2 when [%equal: Value.t list] vs1 vs2 ->
        Some Type_equal.refl
    | Assign a1, Assign a2 when [%equal: Value.t * InstrOp.t] a1 a2 ->
        Some Type_equal.refl
    | _ -> None

  let equal (type c d) (i1 : c t) (i2 : d t) =
    type_equal i1 i2 |> Option.is_some

  module Some = struct
    type t = higher_kinded Instr_make.SomeInstr.t

    let sexp_of_t (Instr_make.SomeInstr.T s) = T.sexp_of_t (project s)
  end

  module Value = Value

  let to_some i = Instr_make.SomeInstr.T (inject i)
  let uses _ = failwith ""
  let defs _ = failwith ""
  let jumps (Control i) = InstrControl.jumps i
end

module Block = struct
  type 'c i = 'c Instr.t

  type 'v t' = {
    entry : ('v, Control.e) Instr.t';
    body : ('v, Control.o) Instr.t' list;
    exit : ('v, Control.c) Instr.t';
  }
  [@@deriving accessors]

  type t = Value.t t'

  let sexp_of_t' f ({ entry; body; exit } : 'v t') =
    [%sexp
      ("entry", (Instr.sexp_of_t' f entry : Sexp.t)),
        ( "body",
          (List.map ~f:(fun i -> Instr.sexp_of_t' f i) body : Sexp.t list) ),
        ("exit", (Instr.sexp_of_t' f exit : Sexp.t))]
end

module Graph = struct
  type 'v t' = {
    entry : Label.t;
    body : 'v Block.t' Label.Map.t;
    exit : Label.t;
  }
  [@@deriving accessors, sexp_of]

  type t = Value.t t'
end
(* module _ : Instr_make.Instr = Instr
   module Instr_make_modules = Instr_make.Make (Instr)
   module Block = Instr_make_modules.Block

   module WithName = Instr_make.MakeTypeModules (struct
     type 'c t = (Name.t, 'c) Instr.t'

     let sexp_of_t i = Instr.sexp_of_t' Name.sexp_of_t i
   end) *)

(* module Graph = Instr_make_modules.Graph *)

let%expect_test _ =
  let v : Value.t = { name = Name.of_string "x"; ty = Ty.U64 } in
  [%sexp_of: Instr.t] (Assign (v, Add (Ty.U64, v, v)))
  |> Sexp.to_string_hum |> printf "%s";
  [%expect
    {|
    (Assign ((name (Name x)) (ty U64))
     (Add (U64 ((name (Name x)) (ty U64)) ((name (Name x)) (ty U64))))) |}]

let%test _ = 1234 = 1234
