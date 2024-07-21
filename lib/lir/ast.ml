open! O
module Name = Utils.Instr_types.Name
module Label = Utils.Instr_types.Label
module Control = Utils.Instr_types.Control

module type Value = sig
  type t [@@deriving sexp_of, compare, hash, equal]

  include Base.Comparable.S with type t := t
end

module Linkage = struct
  type t =
    | Export
    | Preemptible
    | Local
  [@@deriving sexp_of, equal, compare]
end

module Ty = struct
  type t =
    | I1
    | I64
    | Void
    | Ptr
  [@@deriving equal, compare, hash, sexp]
end

module Bin_op = struct
  type t =
    | Add
    | Sub
    | Mul
  [@@deriving sexp]
end

module Cmp_op = struct
  type t =
    | Gt
    | Ge
    | Lt
    | Le
    | Eq
  [@@deriving sexp]
end

module Value = struct
  module Value = struct
    type t =
      { name : Name.t
      ; ty : Ty.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
      }
    [@@deriving equal, compare, hash, sexp, fields]
  end

  include Value
  module Hashtbl = Hashtbl.Make (Value)
  module Hash_set = Hash_set.Make (Value)
  module C = Comparable.Make_plain (Value)
  include C

  let to_int v = Name.to_int v.name
end

module ValueMap = Entity.Map.Make (Value)

module Signed = struct
  type t =
    | Signed
    | Unsigned
  [@@deriving equal, compare, hash, sexp]
end

module Expr = struct
  type 'v t =
    | Bin of
        { ty : Ty.t
        ; op : Bin_op.t
        ; v1 : 'v t
        ; v2 : 'v t
        }
    | Const of
        { ty : Ty.t
        ; const : Z.t
        }
    | Cmp of
        { ty : Ty.t
        ; op : Cmp_op.t
        ; signed : Signed.t
        ; v1 : 'v t
        ; v2 : 'v t
        }
    | Val of
        { ty : Ty.t option
        ; v : 'v
        }
  [@@deriving sexp_of, fold, map, iter]

  let get_ty_with' f = function
    | Bin { ty; _ } | Const { ty; _ } -> ty
    | Val { ty; v } -> f ty v
    | Cmp _ -> I1
  ;;

  let get_ty_with f = get_ty_with' (fun _ty v -> f v)
  let get_ty = get_ty_with (fun v -> v.Value.ty)

  let get_ty_exn v =
    get_ty_with (fun _ -> raise_s [%message "expected non-value expression"]) v
  ;;

  let get_val_exn = function
    | Val { v; _ } -> v
    | e -> raise_s [%message "expected value expression" ~got:(e : _ t)]
  ;;

  let iter_uses i ~f = iter f i
end

module Call = struct
  type 'v t =
    { name : string
    ; args : 'v Expr.t list
    }
  [@@deriving sexp_of, fold, map, iter]

  let map_uses c ~f = map f c
end

module Impure_expr = struct
  type 'v t =
    | Udiv of
        { ty : Ty.t
        ; v1 : 'v Expr.t
        ; v2 : 'v Expr.t
        }
    | Idiv of
        { ty : Ty.t
        ; v1 : 'v Expr.t
        ; v2 : 'v Expr.t
        }
    | Load of
        { ty : Ty.t
        ; ptr : 'v Expr.t
        }
    | Alloca of { size : int32 }
    | Call of
        { ty : Ty.t
        ; call : 'v Call.t
        }
    | Global of { name : string }
  [@@deriving sexp_of, fold, map, iter]

  let get_ty = function
    | Load { ty; _ } | Call { ty; _ } | Udiv { ty; _ } | Idiv { ty; _ } -> ty
    | Alloca _ | Global _ -> Ptr
  ;;

  let iter_uses i ~f = iter f i
end

module Instr = struct
  type 'v t =
    | VoidCall of 'v Call.t
    | Assign of
        { dst : Value.t
        ; expr : 'v Expr.t
        }
    | ImpureAssign of
        { dst : Value.t
        ; expr : 'v Impure_expr.t
        }
    | Store of
        { ty : Ty.t
        ; ptr : 'v Expr.t
        ; expr : 'v Expr.t
        }
  [@@deriving sexp_of, fold, map, iter]

  let has_side_effect = function
    | VoidCall _ | ImpureAssign _ | Store _ -> true
    | Assign _ -> false
  ;;

  let map_uses i ~f = map f i

  let map_defs i ~f =
    match i with
    | Assign { dst; expr } -> Assign { dst = f dst; expr }
    | ImpureAssign { dst; expr } -> ImpureAssign { dst = f dst; expr }
    | VoidCall _ -> i
    | Store _ -> i
  ;;

  let iter_defs i ~f =
    match i with
    | Assign { dst; _ } -> f dst
    | ImpureAssign { dst; _ } -> f dst
    | VoidCall _ | Store _ -> ()
  ;;

  let iter_uses i ~f = iter f i
  (* let to_some i = Some_instr.T (Instr i) *)
end

module Block_call = struct
  type 'v t =
    { label : Label.t
    ; args : 'v Expr.t list
    }
  [@@deriving sexp_of, fields, fold, map, iter]

  let map_uses i ~f = map f i
end

module Block_args = struct
  type t = Value.t list [@@deriving sexp]
end

module Control_instr = struct
  type 'v t =
    | Jump of 'v Block_call.t
    | CondJump of ('v Expr.t * 'v Block_call.t * 'v Block_call.t)
    | Ret of 'v Expr.t option
  [@@deriving sexp_of, fold, map, iter]

  let map_uses i ~f = map f i

  let iter_block_calls i ~f =
    match i with
    | Jump j -> f j
    | CondJump (_, j1, j2) ->
      f j1;
      f j2
    | Ret _ -> ()
  ;;

  let block_calls i = F.Fold.reduce iter_block_calls F.Reduce.to_list_rev i

  let map_block_calls i ~f =
    match i with
    | Jump j -> Jump (f j)
    | CondJump (v, j1, j2) -> CondJump (v, f j1, f j2)
    | Ret v -> Ret v
  ;;

  (* let to_some i = Some_instr.T (Control i) *)
  let iter_uses i ~f = fold (fun () use -> f use) () i
end

module Generic_instr = struct
  type ('v, 'c) t =
    | Block_args : Block_args.t -> ('v, Control.e) t
    | Instr : 'v Instr.t -> ('v, Control.o) t
    | Control : 'v Control_instr.t -> ('v, Control.c) t

  let sexp_of_t (type c v) (f : v -> Sexp.t) (i : (v, c) t) =
    match i with
    | Control c -> [%sexp "Control", (Control_instr.sexp_of_t f c : Sexp.t)]
    | Block_args vs -> [%sexp "Block_args", (vs : Value.t list)]
    | Instr op -> [%sexp "Instr", (Instr.sexp_of_t f op : Sexp.t)]
  ;;

  let fold (type c v) (i : (v, c) t) ~(init : 'a) ~(f : 'a -> v -> 'a) : 'a =
    match i with
    | Block_args _ -> init
    | Instr instr -> Instr.fold f init instr
    | Control c -> Control_instr.fold f init c
  ;;

  let map (type c) f (i : (_, c) t) : (_, c) t =
    match i with
    | Block_args vs -> Block_args vs
    | Instr instr -> Instr (Instr.map f instr)
    | Control c -> Control (Control_instr.map f c)
  ;;

  let get_control : type v. (v, Control.c) t -> v Control_instr.t = function
    | Control c -> c
    (* ocaml can't refute this for some reason? *)
    | _ -> .
  ;;

  let map_control i ~f = Control (f (get_control i))

  let get_instr : type v. (v, Control.o) t -> v Instr.t = function
    | Instr a -> a
    | _ -> .
  ;;

  let map_instr i ~f = Instr (f (get_instr i))

  let block_args_exn : type v. (v, Control.e) t -> Value.t list = function
    | Block_args vs -> vs
    | _ -> .
  ;;

  let map_block_args i ~f = Block_args (f (block_args_exn i))

  let map : type a b c. (a, c) t -> f:(a -> b) -> (b, c) t =
    fun i ~f ->
    match i with
    | Block_args vs -> Block_args vs
    | Instr instr -> Instr (Instr.map f instr)
    | Control c -> Control (Control_instr.map f c)
  ;;

  (* let to_some i = Some_instr.T i *)
  let iter_uses i ~f = fold i ~init:() ~f:(fun () u -> f u)
  let uses i = fold ~init:[] ~f:(Fn.flip List.cons) i
  let map_uses = map

  let map_defs : type c. (_, c) t -> f:(Value.t -> Value.t) -> (_, c) t =
    fun i ~f ->
    match i with
    | Block_args vs -> Block_args (List.map ~f vs)
    | Instr op -> Instr (Instr.map_defs op ~f)
    | Control c -> Control c
  ;;

  let iter_defs (type c) (i : (_, c) t) ~f =
    match i with
    | Block_args vs -> List.iter ~f vs
    | Instr instr -> Instr.iter_defs instr ~f
    | Control _ -> ()
  ;;

  let defs i = F.Fold.reduce iter_defs F.Reduce.to_list_rev i

  let iter_block_calls (type c) (i : (_, c) t) ~f =
    match i with
    | Control c -> Control_instr.iter_block_calls c ~f
    | _ -> ()
  ;;

  let block_calls i = F.Fold.to_list iter_block_calls i
end

module Variant_instr = struct
  type 'v t =
    | Block_args : Block_args.t -> 'v t
    | Instr : 'v Instr.t -> 'v t
    | Control : 'v Control_instr.t -> 'v t
end

module Some_instr = struct
  type 'v t = T : ('v, 'c) Generic_instr.t -> 'v t [@@unboxed]

  let sexp_of_t f (T s) = Generic_instr.sexp_of_t f s
  let map f (T s) = T (Generic_instr.map s ~f)
  let map_uses i ~f = map f i
  let iter_uses (T i) = Generic_instr.iter_uses i
  let iter_defs (T i) = Generic_instr.iter_defs i
  let uses i = F.Fold.reduce iter_uses F.Reduce.to_list_rev i
  let defs i = F.Fold.reduce iter_defs F.Reduce.to_list_rev i

  let has_side_effect = function
    | T (Control _) | T (Block_args _) -> true
    | T (Instr i) -> Instr.has_side_effect i
  ;;

  let to_variant (T instr) : _ Variant_instr.t =
    match instr with
    | Block_args a -> Block_args a
    | Instr i -> Instr i
    | Control i -> Control i
  ;;
end

module Block = struct
  type 'v t =
    { entry : Value.t list
    ; body : 'v Instr.t list
    ; exit : 'v Control_instr.t
    }
  [@@deriving fields, map]

  let sexp_of_t f ({ entry; body; exit } : 'v t) =
    [%sexp
      ("entry", (entry : Value.t list))
      , ("body", (List.sexp_of_t (Instr.sexp_of_t f) body : Sexp.t))
      , ("exit", (Control_instr.sexp_of_t f exit : Sexp.t))]
  ;;

  let map_exit t ~f = { t with exit = f t.exit }

  module Mapper = struct
    type ('a, 'b) t = { f : 'c. ('a, 'c) Generic_instr.t -> ('b, 'c) Generic_instr.t }
  end

  let map_instrs_forwards (m : _ Mapper.t) { entry; body; exit } =
    let entry = m.f (Generic_instr.Block_args entry) |> Generic_instr.block_args_exn in
    let body =
      List.map
        ~f:(fun body -> Generic_instr.Instr body |> m.f |> Generic_instr.get_instr)
        body
    in
    let exit = m.f (Generic_instr.Control exit) |> Generic_instr.get_control in
    { entry; body; exit }
  ;;

  let iter_instrs_forward ({ entry; body; exit } : _ t) ~init ~f =
    let init = f init (Some_instr.T (Generic_instr.Block_args entry)) in
    let init =
      List.fold_left ~init ~f:(fun z i -> f z (Some_instr.T (Generic_instr.Instr i))) body
    in
    f init (Some_instr.T (Generic_instr.Control exit))
  ;;

  let iter_instrs_backward ({ entry; body; exit } : _ t) ~init ~f =
    let init = f init (Some_instr.T (Generic_instr.Control exit)) in
    let init =
      List.rev body
      |> List.fold_left ~init ~f:(fun z i -> f z (Some_instr.T (Generic_instr.Instr i)))
    in
    f init (Some_instr.T (Generic_instr.Block_args entry))
  ;;

  let iter_instrs_forward block ~f =
    iter_instrs_forward block ~init:() ~f:(fun () i -> f i)
  ;;

  let iter_instrs_backward block ~f =
    iter_instrs_backward block ~init:() ~f:(fun () i -> f i)
  ;;

  let iter_jumps (b : _ t) =
    F.Fold.(of_fn exit @> Control_instr.iter_block_calls @> of_fn Block_call.label) b
  ;;

  let jumps (b : _ t) = F.Fold.reduce iter_jumps F.Reduce.to_list_rev b
end

module Graph = struct
  type 'v t = 'v Block.t Cfg.Graph.t [@@deriving sexp_of]

  let map f graph = (Cfg.Graph.map & F.Map.of_map Block.map) graph ~f

  include Cfg.Graph.Make_gen (struct
      type 'a t = 'a Block.t [@@deriving sexp_of]

      let iter_jumps = Block.iter_jumps
    end)

  let validate graph =
    Cfg.Graph.validate graph;
    let preds = predecessors_of_label graph in
    let res = Map.find preds (Cfg.Graph.entry graph) in
    match res with
    | None -> ()
    | Some [ _ ] -> ()
    | Some ls ->
      raise_s
        [%message "the entry label should have no predecessors" ~got:(ls : Label.t list)]
  ;;
end

module Function_ty = struct
  type t =
    { params : Ty.t list
    ; return : Ty.t
    }
  [@@deriving sexp_of]
end

module Named_function_ty = struct
  type t =
    { params : Value.t list
    ; return : Ty.t
    }
  [@@deriving sexp_of]

  let to_anon { params; return } =
    Function_ty.{ params = List.map ~f:(fun { ty; _ } -> ty) params; return }
  ;;
end

module Mut_function = struct
  type 'v t =
    { name : string
    ; linkage : Linkage.t
    ; mutable graph : 'v Graph.t
    ; ty : Named_function_ty.t
    ; mutable unique_label : Label.Id.t
    ; mutable unique_name : Name.Id.t
    }
  [@@deriving sexp_of, fields]

  let fresh_name fn s =
    let unique = fn.unique_name in
    fn.unique_name <- Name.Id.next unique;
    Name.create s unique
  ;;

  let fresh_label fn s =
    let unique = fn.unique_label in
    fn.unique_label <- Label.Id.next unique;
    Label.create s unique
  ;;

  let set_block fn label block = fn.graph <- Cfg.Graph.set label block fn.graph
  let add_block_exn fn label block = fn.graph <- Cfg.Graph.add_exn fn.graph label block
end

module Extern = struct
  type t =
    { name : string
    ; ty : Function_ty.t
    }
  [@@deriving sexp_of]
end

module Function = struct
  type 'v t =
    { name : string
    ; linkage : Linkage.t
    ; graph : 'v Graph.t
    ; ty : Named_function_ty.t
    ; unique_label : Label.Id.t
    ; unique_name : Name.Id.t
    }
  [@@deriving sexp_of, fields, map]

  let map_values fn ~f = map f fn
  let map_graph fn ~f = { fn with graph = f fn.graph }
  (* let map_blocks fn = (map_graph & Cfg.Graph.map_blocks) fn *)

  let iter_blocks fn = Cfg.Graph.iter fn.graph

  let iter_instrs_forward fn =
    F.Fold.(Cfg.Graph.iter @> Block.iter_instrs_forward) fn.graph
  ;;

  let thaw fn =
    { Mut_function.name = fn.name
    ; linkage = fn.linkage
    ; ty = fn.ty
    ; graph = fn.graph
    ; unique_label = fn.unique_label
    ; unique_name = fn.unique_name
    }
  ;;

  let freeze fn =
    { name = fn.Mut_function.name
    ; linkage = fn.linkage
    ; ty = fn.ty
    ; graph = fn.graph
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

module Function_def = struct
  type t =
    { name : string
    ; linkage : Linkage.t
    ; ty : Named_function_ty.t
    }
  [@@deriving sexp_of]
end

module Global_data = struct
  type t =
    | Bytes of string
    | String of string
  [@@deriving sexp_of]
end

module Global = struct
  type t =
    { name : string
    ; linkage : Linkage.t
    ; data : Global_data.t option
    ; align : int (* must be a power of two *)
    }
  [@@deriving sexp_of]
end

module Decl = struct
  type 'v t =
    | Func of 'v Function.t
    | Func_def of Function_def.t
    | Global of Global.t
  [@@deriving sexp_of, map]

  let map_function decl ~f =
    match decl with
    | Func func -> Func (f func)
    | Func_def ty -> Func_def ty
    | Global x -> Global x
  ;;

  let name = function
    | Func func -> func.name
    | Func_def func_def -> func_def.name
    | Global global -> global.name
  ;;

  let linkage = function
    | Func { linkage; _ } | Func_def { linkage; _ } | Global { linkage; _ } -> linkage
  ;;

  let iter_func decl ~f =
    match decl with
    | Func func -> f func
    | _ -> ()
  ;;
end

module Module = struct
  type 'v t = { decls : 'v Decl.t list } [@@deriving sexp_of, fields, map]

  let map_decls { decls } ~f = { decls = List.map decls ~f }
  let map_functions modul ~f = (map_decls & Decl.map_function) modul ~f
  let iter_decls { decls } ~f = List.iter decls ~f

  let get_decl_map modul =
    let res =
      F.Iter.to_list (iter_decls modul)
      |> List.map ~f:(fun decl -> Decl.name decl, decl)
      |> String.Map.of_alist
    in
    match res with
    | `Ok map -> Ok map
    | `Duplicate_key name -> Error name
  ;;
end
