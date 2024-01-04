open O
open Instr_types

module Ty = struct
  type t = U1 | U64 [@@deriving equal, compare, sexp, hash, accessors]
end

module Name = Name
module Label = Label
module Control = Control

module Value = struct
  module T = struct
    type t = {
      name : Name.t;
      ty : Ty.t; [@equal.ignore] [@compare.ignore] [@hash.ignore]
    }
    [@@deriving equal, compare, hash, sexp, accessors]
  end

  include T
  module Hashtbl = Hashtbl.Make (T)
  module Map = Map.Make (T)
  module Set = Set.Make (T)
end

module CmpOp = struct
  type t = Gt [@@deriving equal, compare, sexp, accessors]
end

module InstrOp = struct
  type 'v t' =
    | Add of { ty : Ty.t; v1 : 'v; v2 : 'v }
    | Sub of { ty : Ty.t; v1 : 'v; v2 : 'v }
    | Const of { ty : Ty.t; const : int64 }
    | Cmp of { ty : Ty.t; op : CmpOp.t; v1 : 'v; v2 : 'v }
    | Val of { ty : Ty.t; v : 'v }
  [@@deriving equal, compare, sexp, accessors, variants, map, iter, fold]

  let get_ty = function
    | Add { ty; _ } -> ty
    | Sub { ty; _ } -> ty
    | Const { ty; _ } -> ty
    | Cmp { ty; _ } -> ty
    | Val { ty; _ } -> ty

  type t = Value.t t' [@@deriving equal, compare, sexp]
end

module BlockCall = struct
  type 'v t' = { label : Label.t; args : 'v list }
  [@@deriving equal, compare, sexp, accessors, map, iter, fold]

  type t = Value.t t' [@@deriving equal, compare, sexp]
end

module InstrControl = struct
  type 'v t' =
    | Jump of 'v BlockCall.t'
    | CondJump of ('v * 'v BlockCall.t' * 'v BlockCall.t')
    | Ret of 'v option
  [@@deriving equal, compare, sexp, accessors, map, iter, fold]

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
  end

  include T

  let fold_t' (type c) (f : 'a -> 'v -> 'a) (i : ('v, c) t') ~(init : 'a) : 'a =
    match i with
    | BlockArgs vs -> List.fold_left ~init ~f vs
    | Assign (_, op) -> InstrOp.fold_t' f init op
    | Control c -> InstrControl.fold_t' f init c

  let map_t' (type c) (f : 'v -> 'v) (i : ('v, c) t') : ('v, c) t' =
    match i with
    | BlockArgs vs -> BlockArgs (List.map ~f vs)
    | Assign (v, op) -> Assign (v, InstrOp.map_t' f op)
    | Control c -> Control (InstrControl.map_t' f c)

  type 'c t = (Value.t, 'c) t'

  let get_control : type v. (v, Control.c) t' -> v InstrControl.t' = function
    | Control c -> c
    (* ocaml can't refute this for some reason? *)
    | _ -> assert false

  let get_assign : type v. (v, Control.o) t' -> Value.t * v InstrOp.t' =
    function
    | Assign a -> a
    | _ -> assert false

  let get_args : type v. (v, Control.e) t' -> Value.t list = function
    | BlockArgs vs -> vs
    | _ -> assert false

  let sexp_of_t' (type c v) (f : v -> Sexp.t) (i : (v, c) t') =
    match i with
    | Control c -> [%sexp "Control", (InstrControl.sexp_of_t' f c : Sexp.t)]
    | BlockArgs vs -> [%sexp "BlockArgs", (vs : Value.t list)]
    | Assign (v, op) ->
        [%sexp "Assign", (v : Value.t), (InstrOp.sexp_of_t' f op : Sexp.t)]

  let sexp_of_t instr = sexp_of_t' Value.sexp_of_t instr

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
    type 'v t' = T : ('v, 'c) T.t' -> 'v t'
    type t = Value.t t'

    let sexp_of_t' f (T s) = sexp_of_t' f s
    let sexp_of_t (T s) = sexp_of_t s
  end

  module Value = Value

  let map_t' : type a b c. (a -> b) -> (a, c) t' -> (b, c) t' =
   fun f -> function
    | BlockArgs vs -> BlockArgs vs
    | Assign (v, instr) -> Assign (v, InstrOp.map_t' f instr)
    | Control c -> Control (InstrControl.map_t' f c)

  let to_some i = Some.T i
  let uses i = fold_t' (Fn.flip List.cons) ~init:[] i

  let defs (type c) (i : (_, c) t') =
    match i with BlockArgs vs -> vs | Assign (v, _) -> [ v ] | Control _ -> []

  let jumps i = InstrControl.jumps @@ get_control i
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

  let sexp_of_t block = sexp_of_t' Value.sexp_of_t block
  let jumps (block : t) = Instr.jumps block.exit

  module Mapper = struct
    type ('a, 'b) t = { f : 'c. ('a, 'c) Instr.t' -> ('b, 'c) Instr.t' }
  end

  let map_forwards (m : _ Mapper.t) { entry; body; exit } =
    let entry = m.f entry in
    let body = List.map ~f:m.f body in
    let exit = m.f exit in
    { entry; body; exit }

  let fold_instrs_forward ~init ~f ({ entry; body; exit } : _ t') =
    let init = f init (Instr.to_some entry) in
    let init =
      List.fold_left ~init ~f:(fun z i -> f z (Instr.to_some i)) body
    in
    f init (Instr.to_some exit)

  let fold_instrs_backward ~init ~f ({ entry; body; exit } : _ t') =
    let init = f init (Instr.to_some exit) in
    let init =
      List.rev body
      |> List.fold_left ~init ~f:(fun z i -> f z (Instr.to_some i))
    in
    f init (Instr.to_some entry)

  let instrs_forward_accessor : (_, Instr.Some.t, t, [> many_getter ]) A.t =
    [%accessor
      A.many_getter (fun block ->
          let open A.Many_getter in
          fold_instrs_forward ~init:empty ~f:(fun z i -> access i @ z) block)]
end

let%expect_test _ =
  let (b : Block.t) =
    {
      entry = BlockArgs [];
      body = [];
      exit =
        Instr.Control
          (InstrControl.Ret (Some { name = Name.of_string "x"; ty = U64 }));
    }
  in
  [%sexp_of: Block.t] b |> print_s;
  [%expect
    {|
    ((entry (BlockArgs ())) (body ())
     (exit (Control (Ret (((name (Name x)) (ty U64))))))) |}]

module Graph = struct
  type 'v t' = 'v Block.t' Cfg_graph.Graph.t [@@deriving sexp_of]

  include Cfg_graph.Graph.Accessors

  type t = Value.t t' [@@deriving sexp_of]
end

module Function = struct
  type 'v t' = {
    name : Name.t;
    params : Value.t list;
    body : 'v Graph.t';
    return_ty : Ty.t;
  }
  [@@deriving sexp_of, accessors]

  type t = Value.t t' [@@deriving sexp_of]
end

module DataflowBlock = struct
  type t = Block.t [@@deriving sexp_of]
  type instr = Instr.Some.t

  let jumps = Block.jumps
  let fold_instrs_forward = Block.fold_instrs_forward
  let fold_instrs_backward = Block.fold_instrs_backward
end

module DataflowInstr = struct
  type t = Instr.Some.t [@@deriving sexp_of]

  module Value = Instr.Value

  let uses (Instr.Some.T i) = Instr.uses i
  let defs (Instr.Some.T i) = Instr.defs i
end

module Dataflow = Cfg.MakeDataflowForBlock (DataflowBlock)

module Liveness = struct
  module InstrTransfer = Cfg.MakeLivenessInstrTransfer (DataflowInstr)

  module BlockTransfer =
    Cfg.InstrToBlockTransfer (DataflowBlock) (InstrTransfer)

  include Dataflow.MakeRun (BlockTransfer)
end

module Dominators = struct
  module BlockTransfer = Cfg.MakeDominatorsBlockTransfer (DataflowBlock)
  include Dataflow.MakeRun (BlockTransfer)
end

let%expect_test _ =
  let v : Value.t = { name = Name.of_string "x"; ty = Ty.U64 } in
  [%sexp_of: Instr.t] (Assign (v, Add { ty = Ty.U64; v1 = v; v2 = v }))
  |> Sexp.to_string_hum |> printf "%s";
  [%expect
    {|
    (Assign ((name (Name x)) (ty U64))
     (Add (ty U64) (v1 ((name (Name x)) (ty U64)))
      (v2 ((name (Name x)) (ty U64))))) |}]
