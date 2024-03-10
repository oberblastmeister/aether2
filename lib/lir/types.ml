open! O
include Types_basic
module Name = Utils.Instr_types.Name
module Label = Utils.Instr_types.Label
module Control = Utils.Instr_types.Control

module Value = struct
  include Value
  module Hashtbl = Hashtbl.Make (Value)
  module Hash_set = Hash_set.Make (Value)
  module C = Comparable.Make_plain (Value)
  include C

  let to_int v = Name.to_int v.name
end

module ValueMap = Entity.Map.Make (Value)

module Expr = struct
  include Expr

  let get_ty = function
    | Call { ty; _ } | Bin { ty; _ } | Const { ty; _ } | Val { ty; _ } | Load { ty; _ } ->
      ty
    | Cmp _ -> U1
    | Alloca _ -> todo [%here]
  ;;

  let iter_uses i ~f = iter f i
end

module Instr = struct
  include Instr

  let has_side_effect = function
    | Store _ | Assign { expr = Call _ | Alloca _ | Load _; _ } -> true
    | Assign { expr = Bin _ | Const _ | Cmp _ | Val _; _ } -> false
  ;;

  let map_uses i ~f = map f i

  let map_defs i ~f =
    match i with
    | Assign { dst; expr } -> Assign { dst = f dst; expr }
    | Store _ -> i
  ;;

  let iter_defs i ~f =
    match i with
    | Assign { dst; _ } -> f dst
    | Store _ -> ()
  ;;

  let iter_uses i ~f = iter f i
  let to_some i = Some_instr.T (Instr i)
end

module Block_call = struct
  include Block_call

  let map_uses i ~f = map f i
end

module Control_instr = struct
  include Control_instr

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

  let to_some i = Some_instr.T (Control i)
  let iter_uses i ~f = fold (fun () use -> f use) () i
end

module Block_args = struct
  include Block_args

  let to_some args = Some_instr.T (Block_args args)
end

module Generic_instr = struct
  include Generic_instr

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

  let to_some i = Some_instr.T i
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

module Some_instr = struct
  include Some_instr

  let map_uses i ~f = map f i
  let iter_uses (T i) = Generic_instr.iter_uses i
  let iter_defs (T i) = Generic_instr.iter_defs i
  let uses i = F.Fold.reduce iter_uses F.Reduce.to_list_rev i
  let defs i = F.Fold.reduce iter_defs F.Reduce.to_list_rev i

  let has_side_effect = function
    | T (Control _) | T (Block_args _) -> true
    | T (Instr i) -> Instr.has_side_effect i
  ;;
end

module Block = struct
  include Block

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
    let init = f init (Block_args.to_some entry) in
    let init = List.fold_left ~init ~f:(fun z i -> f z (Instr.to_some i)) body in
    f init (Control_instr.to_some exit)
  ;;

  let iter_instrs_backward ({ entry; body; exit } : _ t) ~init ~f =
    let init = f init (Control_instr.to_some exit) in
    let init =
      List.rev body |> List.fold_left ~init ~f:(fun z i -> f z (Instr.to_some i))
    in
    f init (Block_args.to_some entry)
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
  include Graph

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

module Mut_function = struct
  include Mut_function

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

module Function = struct
  include Function

  let map_graph fn ~f = { fn with graph = f fn.graph }
  (* let map_blocks fn = (map_graph & Cfg.Graph.map_blocks) fn *)

  let iter_instrs_forward fn =
    F.Fold.(Cfg.Graph.iter @> Block.iter_instrs_forward) fn.graph
  ;;

  let thaw fn =
    { Mut_function.name = fn.name
    ; ty = fn.ty
    ; graph = fn.graph
    ; unique_label = fn.unique_label
    ; unique_name = fn.unique_name
    }
  ;;

  let freeze fn =
    { name = fn.Mut_function.name
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

module Program = struct
  include Program

  let map_functions p ~f = { functions = f p.functions }
end
