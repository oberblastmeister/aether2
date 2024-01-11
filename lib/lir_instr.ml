open O
open Instr_types
module T = Lir_instr_types

module InstantiateS = struct
  type t
end

module Ty = struct
  include T.Ty
end

module Name = Name
module Label = Label
module Control = Control

module Value = struct
  include T.Value
  module Hashtbl = Hashtbl.Make (T.Value)
  module Hash_set = Hash_set.Make (T.Value)
  include Comparable.Make (T.Value)
end

module CmpOp = struct
  include T.CmpOp
end

module InstrOp = struct
  include T.InstrOp

  let get_ty = function
    | Add { ty; _ } -> ty
    | Sub { ty; _ } -> ty
    | Const { ty; _ } -> ty
    | Cmp { ty; _ } -> ty
    | Val { ty; _ } -> ty
    | Alloca { ty; _ } -> todo ()
    | Load { ty; _ } -> todo ()
    | Store { ty; _ } -> todo ()
  ;;

  let uses_fold i k = fold (fun () u -> k u) () i
end

module BlockCall = struct
  include T.BlockCall
end

module InstrControl = struct
  include T.InstrControl

  let block_calls_fold i k =
    match i with
    | Jump j -> k j
    | CondJump (_, j1, j2) ->
      k j1;
      k j2
    | Ret _ -> ()
  ;;

  let block_calls i = F.Fold.reduce block_calls_fold F.Reduce.to_list_rev i

  let map_block_calls i ~f =
    match i with
    | Jump j -> Jump (f j)
    | CondJump (v, j1, j2) -> CondJump (v, f j1, f j2)
    | Ret v -> Ret v
  ;;
end

module Instr = struct
  include T.Instr

  let fold (type c v) (i : (v, c) t) ~(init : 'a) ~(f : 'a -> v -> 'a) : 'a =
    match i with
    | Block_args _ -> init
    | Assign (_, op) -> InstrOp.fold f init op
    | Control c -> InstrControl.fold f init c
  ;;

  let map (type c v u) (f : v -> u) (i : (v, c) t) : (u, c) t =
    match i with
    | Block_args vs -> Block_args vs
    | Assign (v, op) -> Assign (v, InstrOp.map f op)
    | Control c -> Control (InstrControl.map f c)
  ;;

  let get_control : type v. (v, Control.c) t -> v InstrControl.t = function
    | Control c -> c
    (* ocaml can't refute this for some reason? *)
    | _ -> assert false
  ;;

  let set_control : type v. (v, Control.c) t -> v InstrControl.t -> (v, Control.c) t =
    fun i c ->
    match i with
    | Control _ -> Control c
    | _ -> assert false
  ;;

  let map_control
    : type v.
      (v, Control.c) t -> f:(v InstrControl.t -> v InstrControl.t) -> (v, Control.c) t
    =
    fun i ~f -> get_control i |> f |> set_control i
  ;;

  let get_assign : type v. (v, Control.o) t -> Value.t * v InstrOp.t = function
    | Assign a -> a
    | _ -> assert false
  ;;

  let set_assign : type v. (v, Control.o) t -> Value.t * v InstrOp.t -> (v, Control.o) t =
    fun i a ->
    match i with
    | Assign _ -> Assign a
    | _ -> assert false
  ;;

  let map_assign
    : type v. (v, Control.o) t -> f:(v InstrOp.t -> v InstrOp.t) -> (v, Control.o) t
    =
    fun i ~f -> get_assign i |> fun (v, op) -> set_assign i (v, f op)
  ;;

  let get_block_args : type v. (v, Control.e) t -> Value.t list = function
    | Block_args vs -> vs
    | _ -> assert false
  ;;

  let set_block_args : type v. (v, Control.e) t -> Value.t list -> (v, Control.e) t =
    fun i vs ->
    match i with
    | Block_args _ -> Block_args vs
    | _ -> assert false
  ;;

  let map_block_args
    : type v. (v, Control.e) t -> f:(Value.t list -> Value.t list) -> (v, Control.e) t
    =
    fun i ~f -> get_block_args i |> fun vs -> set_block_args i (f vs)
  ;;

  let sexp_of_t (type c v) (f : v -> Sexp.t) (i : (v, c) t) =
    match i with
    | Control c -> [%sexp "Control", (InstrControl.sexp_of_t f c : Sexp.t)]
    | Block_args vs -> [%sexp "Block_args", (vs : Value.t list)]
    | Assign (v, op) -> [%sexp "Assign", (v : Value.t), (InstrOp.sexp_of_t f op : Sexp.t)]
  ;;

  (* let type_equal (type c d) (i1 : c t) (i2 : d t) : (c, d) Type_equal.t Option.t =
     match i1, i2 with
     | Control _, Control _ -> Some Type_equal.refl
     | Block_args _, Block_args _ -> Some Type_equal.refl
     | Assign _, Assign _ -> Some Type_equal.refl
     | _ -> None
     ;;

     let tag_equal (type c d) (i1 : c t) (i2 : d t) = type_equal i1 i2 |> Option.is_some *)

  module Some = struct
    type 'v t = T : ('v, 'c) T.Instr.t -> 'v t [@@unboxed]

    let sexp_of_t f (T s) = sexp_of_t f s
  end

  module Value = Value

  let map : type a b c. (a, c) t -> f:(a -> b) -> (b, c) t =
    fun i ~f ->
    match i with
    | Block_args vs -> Block_args vs
    | Assign (v, instr) -> Assign (v, InstrOp.map f instr)
    | Control c -> Control (InstrControl.map f c)
  ;;

  let to_some i = Some.T i
  let uses_fold (Some.T i) k = fold i ~init:() ~f:(fun () u -> k u)
  let uses i = fold ~init:[] ~f:(Fn.flip List.cons) i
  let map_uses = map

  let map_defs : type c. (_, c) t -> f:(Value.t -> Value.t) -> (_, c) t =
    fun i ~f ->
    match i with
    | Block_args vs -> Block_args (List.map ~f vs)
    | Assign (v, op) -> Assign (f v, op)
    | Control c -> Control c
  ;;

  let defs_fold (Some.T i) k =
    match i with
    | Block_args vs -> List.iter ~f:k vs
    | Assign (v, _) -> k v
    | Control _ -> ()
  ;;

  let defs i = F.Fold.reduce defs_fold F.Reduce.to_list_rev (Some.T i)
  let jumps i = InstrControl.block_calls @@ get_control i
end

module Block = struct
  include T.Block

  let map_exit t ~f = { t with exit = Instr.map_control t.exit ~f }

  let sexp_of_t f ({ entry; body; exit } : 'v t) =
    [%sexp
      ("entry", (Instr.sexp_of_t f entry : Sexp.t))
      , ("body", (List.map ~f:(fun i -> Instr.sexp_of_t f i) body : Sexp.t list))
      , ("exit", (Instr.sexp_of_t f exit : Sexp.t))]
  ;;

  module Mapper = struct
    type ('a, 'b) t = { f : 'c. ('a, 'c) Instr.t -> ('b, 'c) Instr.t }
  end

  let map_instrs_forwards (m : _ Mapper.t) { entry; body; exit } =
    let entry = m.f entry in
    let body = List.map ~f:m.f body in
    let exit = m.f exit in
    { entry; body; exit }
  ;;

  let fold_instrs_forward ({ entry; body; exit } : _ t) ~init ~f =
    let init = f init (Instr.to_some entry) in
    let init = List.fold_left ~init ~f:(fun z i -> f z (Instr.to_some i)) body in
    f init (Instr.to_some exit)
  ;;

  let fold_instrs_backward ({ entry; body; exit } : _ t) ~init ~f =
    let init = f init (Instr.to_some exit) in
    let init =
      List.rev body |> List.fold_left ~init ~f:(fun z i -> f z (Instr.to_some i))
    in
    f init (Instr.to_some entry)
  ;;

  let instrs_forward_fold block k =
    fold_instrs_forward block ~init:() ~f:(fun () i -> k i)
  ;;

  let jumps_fold (b : _ t) =
    F.Fold.(
      of_fn exit
      @> of_fn Instr.get_control
      @> InstrControl.block_calls_fold
      @> of_fn BlockCall.label)
      b
  ;;

  let jumps (b : _ t) = F.Fold.reduce jumps_fold F.Reduce.to_list_rev b
end

module Graph = struct
  include T.Graph
  include Cfg_graph.Graph.Stuff

  let predecessors_of_label b = predecessors_of_label ~jumps:Block.jumps b

  let validate graph =
    validate graph;
    let preds = predecessors_of_label graph in
    let res = Map.find preds graph.entry in
    match res with
    | None -> ()
    | Some [ _ ] -> ()
    | Some ls ->
      raise_s
        [%message "the entry label should have no predecessors" ~got:(ls : Label.t list)]
  ;;

  module MakeDataGraph (V : sig
      type t [@@deriving sexp_of]
    end) : Data_graph.SingleEntryGraph with type t = V.t t and module Node = Label =
  MakeDataGraph (struct
      type t = V.t Block.t [@@deriving sexp_of]

      let jumps_fold b = Block.jumps_fold b
      let jumps b = Block.jumps b
    end)

  module DataGraph :
    Data_graph.SingleEntryGraph with type t = Value.t t and module Node = Label = struct
    include MakeDataGraph (struct
        type t = Value.t [@@deriving sexp_of]
      end)
  end

  module Dfs = struct
    include Data_graph.Dfs (DataGraph)
  end
end

module MutFunction = struct
  type 'v t =
    { name : string
    ; params : Value.t list
    ; mutable graph : 'v Graph.t
    ; return_ty : Ty.t
    ; mutable unique_label : int
    ; mutable unique_name : int
    }
  [@@deriving sexp_of]

  let fresh_name fn s =
    let unique = fn.unique_name in
    fn.unique_name <- unique + 1;
    Name.Unique { name = s; unique }
  ;;

  let fresh_label fn s =
    let unique = fn.unique_label in
    fn.unique_label <- unique + 1;
    { Label.name = Name.Unique { name = s; unique } }
  ;;

  let set_block fn label block = fn.graph <- Graph.set_block fn.graph label block
  let add_block_exn fn label block = fn.graph <- Graph.add_block_exn fn.graph label block
end

module Function = struct
  type 'v t =
    { name : string
    ; params : Value.t list
    ; graph : 'v Graph.t
    ; return_ty : Ty.t
    ; unique_label : int
    ; unique_name : int
    }
  [@@deriving sexp_of, fields]

  let map_body fn ~f = { fn with graph = f fn.graph }
  let map_blocks fn = (map_body & Graph.map_blocks) fn

  let instrs_forward_fold fn =
    F.Fold.(FC.Map.fold @> Block.instrs_forward_fold) fn.graph.blocks
  ;;

  let thaw fn =
    { MutFunction.name = fn.name
    ; params = fn.params
    ; graph = fn.graph
    ; return_ty = fn.return_ty
    ; unique_label = fn.unique_label
    ; unique_name = fn.unique_name
    }
  ;;

  let freeze fn =
    { name = fn.MutFunction.name
    ; params = fn.params
    ; graph = fn.graph
    ; return_ty = fn.return_ty
    ; unique_label = fn.unique_label
    ; unique_name = fn.unique_name
    }
  ;;

  let with_mut fn f =
    let mut_fn = thaw fn in
    f mut_fn;
    freeze mut_fn
  ;;
end

module Program = struct
  type 'v t = { functions : 'v Function.t list } [@@deriving sexp_of, fields]

  let map_functions p ~f = { p with functions = f p.functions }
end

let%expect_test _ =
  let v : Value.t = { name = Name.of_string "x"; ty = Ty.U64 } in
  [%sexp_of: Value.t Instr.t] (Assign (v, Add { ty = Ty.U64; v1 = v; v2 = v }))
  |> Sexp.to_string_hum
  |> printf "%s";
  [%expect
    {|
    (Assign ((name (Name x)) (ty U64))
     (Add (ty U64) (v1 ((name (Name x)) (ty U64)))
      (v2 ((name (Name x)) (ty U64))))) |}]
;;

module Instantiate (V : sig
    type t [@@deriving sexp_of]
  end) =
struct
  module InstrOp = struct
    type t = V.t InstrOp.t [@@deriving sexp_of]
  end

  module BlockCall = struct
    type t = V.t BlockCall.t [@@deriving sexp_of]
  end

  module InstrControl = struct
    type t = V.t InstrControl.t [@@deriving sexp_of]
  end

  module Instr = struct
    type 'c t = ('c, V.t) Instr.t

    module Some = struct
      type t = V.t Instr.Some.t [@@deriving sexp_of]
    end
  end

  module Block = struct
    type t = V.t Block.t
  end

  module DataGraph = Graph.MakeDataGraph (V)
  module Dfs = Data_graph.Dfs (DataGraph)

  module Graph = struct
    type t = V.t Graph.t [@@deriving sexp_of]
  end

  module MutFunction = struct
    type t = V.t MutFunction.t
  end

  module Function = struct
    type t = V.t Function.t [@@deriving sexp_of]
  end

  module Program = struct
    type t = V.t Program.t [@@deriving sexp_of]
  end
end

module InstantiateWithDataflow (V : sig
    type t [@@deriving equal, compare, hash, sexp]

    include Comparable.S with type t := t
  end) =
struct
  module M = Instantiate (V)

  module Dataflow = struct
    module Instr = struct
      module Value = V

      type t = V.t Instr.Some.t [@@deriving sexp_of]

      let uses (Instr.Some.T i : t) = Instr.uses i
      let defs (Instr.Some.T i : t) = Instr.defs i
    end

    module Block = struct
      include Block

      type t = V.t Block.t [@@deriving sexp_of]

      module Node = Label
      module Instr = Instr
    end

    module Framework = Cfg.Dataflow.Make (struct
        include M.DataGraph
        module Block = Block

        let exit (graph : M.Graph.t) = graph.exit
        let exit (graph : M.Graph.t) = graph.exit
        let get_block (graph : M.Graph.t) label = Map.find_exn graph.blocks label
      end)
  end

  module DataflowDominators = struct
    module BlockTransfer = Cfg.Dataflow.Dominators.Make (Dataflow.Block)
    include Cfg.Dataflow.Dominators.MakeHelpers (Dataflow.Block)
    include Dataflow.Framework.MakeAnalysis (BlockTransfer)
  end

  module Dominators = Dominators.MakeDominators (Graph.DataGraph)
  include M
end

module Vir = struct
  include InstantiateWithDataflow (struct
      include Value
    end)

  module Liveness = struct
    module InstrTransfer = Cfg.Dataflow.Liveness.Make (Dataflow.Instr)

    module BlockTransfer =
      Cfg.Dataflow.InstrToBlockTransfer (Dataflow.Block) (InstrTransfer)

    include Dataflow.Framework.MakeAnalysis (BlockTransfer)
  end
end

module Dominators = Dominators.MakeDominators (Graph.DataGraph)
