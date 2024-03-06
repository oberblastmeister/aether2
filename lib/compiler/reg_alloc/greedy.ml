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
  |> F.Iter.filter ~f:(fun node -> not @@ Name.Table.mem precolored node)
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
  Entity.Map.to_iteri precolored
  |> F.Iter.iter ~f:(fun (node, _) -> increase_neighbor_weights node);
  F.Iter.unfoldr
    (fun _ ->
      let open Option.Let_syntax in
      let%bind node, _weight = IntHeap.pop heap in
      increase_neighbor_weights node;
      Some (node, ()))
    ()
;;

let color_with ~interference ~ordering =
  let color_of_name = Name.Table.create () in
  let max_color = ref Color.lowest in
  let used_colors = Hash_set.create (module Color) in
  ordering
  |> F.Iter.iter ~f:(fun name ->
    let neighbor_colors =
      Interference.neighbors interference name
      |> F.Iter.map ~f:(fun neighbor ->
        (* might not be in the map if precolored *)
        Name.Table.find color_of_name neighbor |> FC.Option.fold)
      |> F.Iter.concat
      |> F.Iter.to_array
    in
    Array.sort neighbor_colors ~compare:Color.compare;
    (* TODO: also try to pick colors that don't have lots of register constraints *)
    (* TODO: filter out colors that are impossible to register allocate *)
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
    Hash_set.add used_colors lowest_not_in_neighbors;
    ());
  color_of_name, !max_color
;;

let collect_precolored_constraints (type r) ~dict ~interference ~color_of_name ~precolored
  =
  let register = dict.register in
  let enum = register.enum in
  let open Result.Let_syntax in
  let module E = struct
    exception InvalidRegisterConstraint of r * r
  end
  in
  let register_constraints_of_color = ColorMap.create () in
  let%bind () =
    match
      Interference.nodes interference (fun node ->
        Interference.neighbors interference node (fun neighbor ->
          (match Name.Table.find precolored node, Name.Table.find precolored neighbor with
           | Some reg, Some reg' ->
             if register.equal reg reg'
             then raise (E.InvalidRegisterConstraint (reg, reg'))
             else ()
           | None, Some reg ->
             let node_color = Name.Table.find_exn color_of_name node in
             (match ColorMap.find register_constraints_of_color node_color with
              | None ->
                ColorMap.set
                  register_constraints_of_color
                  ~key:node_color
                  ~data:
                    (Data.Enum_set.create ~enum ()
                     |- Fn.flip (Data.Enum_set.add ~enum) reg)
              | Some set -> Data.Enum_set.add ~enum set reg)
           | None, None -> ()
           | Some _, None -> ());
          ());
        ())
    with
    | exception E.InvalidRegisterConstraint (reg, reg') ->
      Or_error.error_s
        [%message
          "invalid register constraint"
            (register.sexp_of reg : Sexp.t)
            (register.sexp_of reg' : Sexp.t)]
    | _ -> Ok ()
  in
  Ok register_constraints_of_color
;;

let color ~interference ~precolored =
  color_with
    ~interference
    ~ordering:(simplicial_elimination_ordering ~interference ~precolored)
;;

let alloc_colors ~dict ~precolored ~interference =
  let { register; config } = dict in
  let enum = register.enum in
  let open Result.Let_syntax in
  let used_registers = Data.Enum_set.create ~enum () in
  let color_of_name, max_color = color ~interference ~precolored in
  let%bind register_constraints_of_color =
    collect_precolored_constraints ~dict ~interference ~color_of_name ~precolored
  in
  let alloc_of_color = ColorMap.create () in
  F.Iter.(
    (* TODO: fix that colors in between 0 and max_color aren't used *)
    Color.to_int Color.lowest -- Color.to_int max_color
    |> iter ~f:(fun color ->
      let color = Color.of_int color in
      let register_constraints = ColorMap.find register_constraints_of_color color in
      let reg =
        F.Iter.(
          of_list dict.config.register_order
          |> F.Iter.find_pred ~f:(fun reg ->
            let not_in_constraints =
              Option.value_map
                register_constraints
                ~default:true
                ~f:(fun register_constraints ->
                  not (Data.Enum_set.mem ~enum register_constraints reg))
            in
            let not_used = not (Data.Enum_set.mem ~enum used_registers reg) in
            not_in_constraints && not_used))
      in
      let alloc_reg =
        reg |> Option.value_map ~default:Alloc_reg.Spilled ~f:Alloc_reg.inreg
      in
      Option.iter reg ~f:(Data.Enum_set.add ~enum used_registers);
      ColorMap.set alloc_of_color ~key:color ~data:alloc_reg;
      ()));
  Ok (color_of_name, alloc_of_color, used_registers)
;;

let run ~dict ~precolored ~interference ~constraints =
  let open Result.Let_syntax in
  let%bind color_of_name, alloc_of_color, used_registers =
    alloc_colors ~dict ~precolored ~interference
  in
  let alloc_of_name =
    Entity.Map.to_iteri color_of_name
    |> F.Iter.map ~f:(fun (name, color) ->
      let alloc = ColorMap.find_exn alloc_of_color color in
      name, alloc)
    |> Name.Table.of_iter ~size:(Interference.size interference)
  in
  Ok { alloc_of_name; used_registers }
;;
