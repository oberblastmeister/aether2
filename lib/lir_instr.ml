include Lir_instr_intf
open! O
open Instr_types
module T = Lir_instr_types

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

module Cmp_op = struct
  include T.Cmp_op
end

module Bin_op = struct
  include T.Bin_op
end

module Expr = struct
  include T.Expr

  let get_ty = function
    | Bin { ty; _ } -> ty
    | Const { ty; _ } -> ty
    | Cmp _ -> U1
    | Val { ty; _ } -> ty
    | Alloca { ty; _ } -> todo ()
    | Load { ty; _ } -> todo ()
  ;;
end

module Instr = struct
  include T.Instr

  let map_defs i ~f =
    match i with
    | Assign { dst; expr } -> Assign { dst = f dst; expr }
    | Store _ -> i
  ;;

  let defs_fold i k =
    match i with
    | Assign { dst; _ } -> k dst
    | Store _ -> ()
  ;;

  let uses_fold i k = fold (fun () u -> k u) () i
  let to_some i = T.Some_instr.T (Instr i)
end

module Block_call = struct
  include T.Block_call
end

module Control_instr = struct
  include T.Control_instr

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

  let to_some i = T.Some_instr.T (Control i)
end

module Block_args = struct
  include T.Block_args

  let to_some args = T.Some_instr.T (Block_args args)
end

module Generic_instr = struct
  include T.Generic_instr

  let fold (type c v) (i : (v, c) t) ~(init : 'a) ~(f : 'a -> v -> 'a) : 'a =
    match i with
    | Block_args _ -> init
    | Instr instr -> Instr.fold f init instr
    | Control c -> Control_instr.fold f init c
  ;;

  let map (type c v u) (f : v -> u) (i : (v, c) t) : (u, c) t =
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

  let get_block_args : type v. (v, Control.e) t -> Value.t list = function
    | Block_args vs -> vs
    | _ -> .
  ;;

  let map_block_args i ~f = Block_args (f (get_block_args i))

  let map : type a b c. (a, c) t -> f:(a -> b) -> (b, c) t =
    fun i ~f ->
    match i with
    | Block_args vs -> Block_args vs
    | Instr instr -> Instr (Instr.map f instr)
    | Control c -> Control (Control_instr.map f c)
  ;;

  let to_some i = T.Some_instr.T i
  let uses_fold i k = fold i ~init:() ~f:(fun () u -> k u)
  let uses i = fold ~init:[] ~f:(Fn.flip List.cons) i
  let map_uses = map

  let map_defs : type c. (_, c) t -> f:(Value.t -> Value.t) -> (_, c) t =
    fun i ~f ->
    match i with
    | Block_args vs -> Block_args (List.map ~f vs)
    | Instr op -> Instr (Instr.map_defs op ~f)
    | Control c -> Control c
  ;;

  let defs_fold (type c) (i : (_, c) t) k =
    match i with
    | Block_args vs -> List.iter ~f:k vs
    | Instr instr -> Instr.defs_fold instr k
    | Control _ -> ()
  ;;

  let defs i = F.Fold.reduce defs_fold F.Reduce.to_list_rev i

  let block_calls_fold (type c) (i : (_, c) t) k =
    match i with
    | Control c -> Control_instr.block_calls_fold c k
    | _ -> ()
  ;;

  let block_calls i = F.Fold.to_list block_calls_fold i
end

module Some_instr = struct
  include T.Some_instr

  let uses_fold (T i) = Generic_instr.uses_fold i
  let defs_fold (T i) = Generic_instr.defs_fold i
  let uses i = F.Fold.reduce uses_fold F.Reduce.to_list_rev i
  let defs i = F.Fold.reduce defs_fold F.Reduce.to_list_rev i
end

module Block = struct
  include T.Block

  let map_exit t ~f = { t with exit = f t.exit }

  module Mapper = struct
    type ('a, 'b) t = { f : 'c. ('a, 'c) Generic_instr.t -> ('b, 'c) Generic_instr.t }
  end

  let map_instrs_forwards (m : _ Mapper.t) { entry; body; exit } =
    let entry = m.f (Generic_instr.Block_args entry) |> Generic_instr.get_block_args in
    let body =
      List.map
        ~f:(fun body -> Generic_instr.Instr body |> m.f |> Generic_instr.get_instr)
        body
    in
    let exit = m.f (Generic_instr.Control exit) |> Generic_instr.get_control in
    { entry; body; exit }
  ;;

  let fold_instrs_forward ({ entry; body; exit } : _ t) ~init ~f =
    let init = f init (Block_args.to_some entry) in
    let init = List.fold_left ~init ~f:(fun z i -> f z (Instr.to_some i)) body in
    f init (Control_instr.to_some exit)
  ;;

  let fold_instrs_backward ({ entry; body; exit } : _ t) ~init ~f =
    let init = f init (Control_instr.to_some exit) in
    let init =
      List.rev body |> List.fold_left ~init ~f:(fun z i -> f z (Instr.to_some i))
    in
    f init (Block_args.to_some entry)
  ;;

  let instrs_forward_fold block k =
    fold_instrs_forward block ~init:() ~f:(fun () i -> k i)
  ;;

  let instrs_backward_fold block k =
    fold_instrs_backward block ~init:() ~f:(fun () i -> k i)
  ;;

  let jumps_fold (b : _ t) =
    F.Fold.(of_fn exit @> Control_instr.block_calls_fold @> of_fn Block_call.label) b
  ;;

  let jumps (b : _ t) = F.Fold.reduce jumps_fold F.Reduce.to_list_rev b
end

module Graph = struct
  include Cfg_graph
  include T.Graph

  let predecessors_of_label b = Cfg_graph.predecessors_of_label ~jumps:Block.jumps b
  let to_graph g = Cfg_graph.to_graph ~jumps:Block.jumps_fold g

  let to_double_graph g =
    to_graph g |> Data_graph.double_of_t (Constructors.some_hashtbl (module Label))
  ;;

  let validate graph =
    Cfg_graph.validate graph;
    let preds = predecessors_of_label graph in
    let res = Map.find preds graph.entry in
    match res with
    | None -> ()
    | Some [ _ ] -> ()
    | Some ls ->
      raise_s
        [%message "the entry label should have no predecessors" ~got:(ls : Label.t list)]
  ;;

  let get_idoms (graph : _ t) = Dominators.get_idoms graph.entry @@ to_double_graph graph
end

module Dataflow = struct
  let instr_to_block_transfer (type a) (module Value : Value with type t = a) =
    Dataflow.instr_to_block_transfer
      (module struct
        type t = Value.t Block.t [@@deriving sexp_of]
      end)
      { instrs_forward_fold = Block.instrs_forward_fold
      ; instrs_backward_fold = Block.instrs_backward_fold
      }
  ;;

  let run_block_transfer transfer (graph : _ Graph.t) =
    Dataflow.run_block_transfer
      transfer
      { entry = graph.entry
      ; v = Graph.to_double_graph graph
      ; exit = graph.exit
      ; get_block = Map.find_exn graph.blocks
      }
  ;;
end

module Mut_function = struct
  include T.Mut_function

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
  include T.Function

  let map_graph fn ~f = { fn with graph = f fn.graph }
  let map_blocks fn = (map_graph & Graph.map_blocks) fn

  let instrs_forward_fold fn =
    F.Fold.(FC.Map.fold @> Block.instrs_forward_fold) fn.graph.blocks
  ;;

  let thaw fn =
    { Mut_function.name = fn.name
    ; params = fn.params
    ; graph = fn.graph
    ; return_ty = fn.return_ty
    ; unique_label = fn.unique_label
    ; unique_name = fn.unique_name
    }
  ;;

  let freeze fn =
    { name = fn.Mut_function.name
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
  include T.Program

  let map_functions p ~f = { p with functions = f p.functions }
end

module Testing : sig
  type 'a t

  val takes_plist : int t -> int t
end = struct
  type 'a t = ('a * 'a) list

  let takes_plist = List.map ~f:(fun (x, y) -> x + 1, y + 1)
end
