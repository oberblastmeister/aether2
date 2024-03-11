(* TODO: use precolored registers that have also set machine registers *)
(* only find registers for non precolored registers *)
open O
open Utils.Instr_types
open Types

module Color = struct
  include Entity.Id.Make ()

  let lowest = of_int 0
end

module ColorMap = Entity.Map.Make (Color)

module Coloring = struct
  type t = (Name.t, Color.t) Entity.Map.t [@@deriving sexp_of]
end

module Heap = Data.Array_heap.Indexed

module IntHeap = Heap.Make (struct
    module T = struct
      type t = Name.t * int [@@deriving sexp]

      let compare x y = Int.compare (snd x) (snd y)
    end

    include T
    include Comparator.Make (T)
  end)

open Config

let simplicial_elimination_ordering ~interference ~precolored =
  let heap = IntHeap.create ~size:(Interference.size interference) () in
  interference
  |> Interference.nodes
  |> F.Iter.filter ~f:(fun node -> not @@ Map.mem precolored node)
  |> F.Iter.iter ~f:(fun name ->
    IntHeap.set heap ~key:(Name.Id.to_int @@ Name.to_id name) ~data:(name, 0));
  let increase_neighbor_weights node =
    Interference.neighbors interference node
    |> F.Iter.iter ~f:(fun neighbor ->
      (* will only modify if it is on the heap *)
      IntHeap.modify
        heap
        ~key:(Name.Id.to_int @@ Name.to_id neighbor)
        ~f:(Tuple2.map_snd ~f:succ))
  in
  Map.iter_keys precolored ~f:(fun node -> increase_neighbor_weights node);
  (* |> F.Iter.iter ~f:(fun (node, _) -> increase_neighbor_weights node); *)
  F.Iter.unfoldr ~init:() ~f:(fun () ->
    let open Option.Let_syntax in
    let%bind node, _weight = IntHeap.pop heap in
    increase_neighbor_weights node;
    Some (node, ()))
;;

let color_with ~interference ~precolored ~ordering =
  let color_of_name = Name.Table.create () in
  let max_color = ref @@ Color.of_int 0 in
  (* TODO: sort these based on the register order *)
  Map.iter_keys precolored ~f:(fun name ->
    Name.Table.set color_of_name ~key:name ~data:!max_color;
    max_color := Color.next !max_color;
    ());
  if not Color.(!max_color = of_int 0) then max_color := Color.prev !max_color;
  ordering
  |> F.Iter.iter ~f:(fun name ->
    let neighbor_colors =
      Interference.neighbors interference name
      |> F.Iter.concat_map ~f:(fun neighbor ->
        (* the neighbor may not have been colored yet *)
        Name.Table.find color_of_name neighbor |> Option.iter)
      |> F.Iter.to_array
    in
    Array.sort neighbor_colors ~compare:Color.compare;
    let lowest_not_in_neighbors =
      Array.fold_until
        neighbor_colors
        ~init:Color.lowest
        ~f:(fun current_color neighbor_color ->
          (* our array is sorted *)
          assert (Color.(current_color <= neighbor_color));
          if Color.(current_color = neighbor_color)
          then Continue_or_stop.Continue (Color.next current_color)
          else Continue_or_stop.Stop current_color)
        ~finish:Fn.id
    in
    Name.Table.set color_of_name ~key:name ~data:lowest_not_in_neighbors;
    max_color := Color.max !max_color lowest_not_in_neighbors;
    ());
  color_of_name, !max_color
;;

let color ~interference ~precolored =
  color_with
    ~interference
    ~precolored
    ~ordering:(simplicial_elimination_ordering ~interference ~precolored)
;;

let alloc_colors ~dict ~precolored ~interference =
  let { register; config } = dict in
  let enum = register.enum in
  let used_registers = Data.Enum_set.create ~enum () in
  (* immediately add all precolored registers to the used set *)
  [%log.global.debug
    "adding all precolored registers to used"
      (Name.Map.sexp_of_t register.sexp_of precolored : Sexp.t)];
  Map.iter precolored ~f:(fun reg -> Data.Enum_set.add ~enum used_registers reg);
  let color_of_name, max_color = color ~interference ~precolored in
  [%log.global.debug (color_of_name : Color.t Name.Table.t)];
  let alloc_of_color = ColorMap.create () in
  (* immediately set the colors of precolored registers *)
  Map.iteri precolored ~f:(fun ~key:name ~data:reg ->
    let color = Name.Table.find_exn color_of_name name in
    ColorMap.set alloc_of_color ~key:color ~data:(Alloc_reg.inreg reg);
    ());
  [%log.global.debug
    (ColorMap.sexp_of_t (Alloc_reg.sexp_of_t register.sexp_of) alloc_of_color : Sexp.t)];
  F.Iter.Infix.(
    Color.to_int Color.lowest -- Color.to_int max_color
    |> F.Iter.map ~f:Color.of_int
    |> F.Iter.filter ~f:(fun color -> not @@ ColorMap.mem alloc_of_color color)
    (* make sure they aren't precolored *)
    |> F.Iter.iter ~f:(fun color ->
      let reg =
        F.Iter.(
          List.iter config.register_order
          |> F.Iter.find ~f:(fun reg ->
            let not_used = not (Data.Enum_set.mem ~enum used_registers reg) in
            not_used))
      in
      let alloc_reg =
        reg |> Option.value_map ~default:Alloc_reg.Spilled ~f:Alloc_reg.inreg
      in
      Option.iter reg ~f:(Data.Enum_set.add ~enum used_registers);
      ColorMap.set alloc_of_color ~key:color ~data:alloc_reg;
      [%log.global.debug
        "setting color" (color : Color.t) (Option.sexp_of_t register.sexp_of reg : Sexp.t)];
      ()));
  color_of_name, alloc_of_color, used_registers
;;

let run ~dict ~precolored ~interference =
  let open Result.Let_syntax in
  let precolored = Map.of_alist_exn (module Name) precolored in
  let color_of_name, alloc_of_color, used_registers =
    alloc_colors ~dict ~precolored ~interference
  in
  let alloc_of_name =
    Entity.Map.iteri color_of_name
    (* make sure not to include precolored nodes in the allocation *)
    |> F.Iter.filter ~f:(fun (name, _) -> not @@ Map.mem precolored name)
    |> F.Iter.map ~f:(fun (name, color) ->
      let alloc = ColorMap.find_exn alloc_of_color color in
      name, alloc)
    |> Name.Table.of_iter ~size:(Interference.size interference)
  in
  { alloc_of_name; used_registers }
;;

