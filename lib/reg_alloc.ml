open O
open Utils.Instr_types
module NameMap = Entity.Map.Make (Name)
include Reg_alloc_intf

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

module Make (Register : Register) (RegisterSet : Set with type value = Register.t) =
struct
  module Alloc_reg = struct
    type t =
      | InReg of Register.t
      | Spilled
    [@@deriving sexp_of, variants]
  end

  type error = InvalidRegisterConstraint of Register.t * Register.t

  let simplicial_elimination_ordering interference ~precolored =
    let heap = IntHeap.create ~size:(Interference.size interference) () in
    interference
    |> Interference.nodes
    |> F.Iter.filter ~f:(fun node -> not @@ NameMap.mem precolored node)
    |> F.Iter.iter ~f:(fun name ->
      IntHeap.set heap ~key:(Name.Id.to_int @@ Name.to_id name) ~data:(name, 0));
    let increase_neighbor_weights node =
      Interference.neighbors interference node
      |> F.Iter.iter ~f:(fun neighbor ->
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

  let color_with interference ordering =
    let color_of_name = NameMap.create () in
    let max_color = ref Color.lowest in
    ordering
    |> F.Iter.iter ~f:(fun name ->
      let neighbor_colors =
        Interference.neighbors interference name
        |> F.Iter.map ~f:(fun neighbor ->
          (* might not be in the map if precolored *)
          NameMap.find color_of_name neighbor |> FC.Option.fold)
        |> F.Iter.concat
        |> F.Iter.to_array
      in
      Array.sort neighbor_colors ~compare:Color.compare;
      let lowest =
        Array.fold_until
          neighbor_colors
          ~init:Color.lowest
          ~f:(fun lowest color ->
            (* our array is sorted *)
            assert (Color.(lowest <= color));
            if Color.(lowest = color)
            then Continue_or_stop.Continue (Color.next color)
            else Continue_or_stop.Stop lowest)
          ~finish:Fn.id
      in
      NameMap.set color_of_name ~key:name ~data:lowest;
      max_color := Color.max !max_color lowest;
      ());
    color_of_name, !max_color
  ;;

  let collect_precolored_constraints ~interference ~color_of_name ~precolored =
    let open Result.Let_syntax in
    let module E = struct
      exception InvalidRegisterConstraint of Register.t * Register.t
    end
    in
    let register_constraints_of_color = ColorMap.create () in
    let%bind () =
      match
        Interference.nodes interference (fun node ->
          Interference.neighbors interference node (fun neighbor ->
            (match NameMap.find precolored node, NameMap.find precolored neighbor with
             | Some reg, Some reg' ->
               if Register.(equal reg reg')
               then raise (E.InvalidRegisterConstraint (reg, reg'))
               else ()
             | None, Some reg ->
               let node_color = NameMap.find_exn color_of_name node in
               (match ColorMap.find register_constraints_of_color node_color with
                | None ->
                  ColorMap.set
                    register_constraints_of_color
                    ~key:node_color
                    ~data:(RegisterSet.create () |- Fn.flip RegisterSet.add reg)
                | Some set -> RegisterSet.add set reg)
             | None, None -> ()
             | Some _, None -> ());
            ());
          ())
      with
      | exception E.InvalidRegisterConstraint (reg, reg') ->
        Error (InvalidRegisterConstraint (reg, reg'))
      | _ -> Ok ()
    in
    Ok register_constraints_of_color
  ;;

  let color interference ~precolored =
    color_with interference (simplicial_elimination_ordering interference ~precolored)
  ;;

  let alloc_colors ~precolored ~register_order interference =
    let open Result.Let_syntax in
    let used_registers = RegisterSet.create () in
    let color_of_name, max_color = color interference ~precolored in
    let%bind register_constraints_of_color =
      collect_precolored_constraints ~interference ~color_of_name ~precolored
    in
    let alloc_of_color = ColorMap.create () in
    F.Iter.(
      Color.to_int Color.lowest -- Color.to_int max_color
      |> iter ~f:(fun color ->
        let color = Color.of_int color in
        let register_constraints =
          ColorMap.find_exn register_constraints_of_color color
        in
        let reg =
          F.Iter.(
            of_list register_order
            |> F.Iter.find_pred ~f:(fun reg ->
              (not (RegisterSet.mem register_constraints reg))
              && not (RegisterSet.mem used_registers reg)))
        in
        let alloc_reg =
          reg |> Option.value_map ~default:Alloc_reg.Spilled ~f:Alloc_reg.inreg
        in
        ColorMap.set alloc_of_color ~key:color ~data:alloc_reg;
        ()));
    Ok (color_of_name, alloc_of_color, used_registers)
  ;;

  module Allocation = struct
    type t =
      { alloc_of_name : (Name.t, Alloc_reg.t) Entity.Map.t
      ; used_registers : RegisterSet.t
      }
    [@@deriving sexp_of]

    let find_exn _ = todo ()
    let did_use_reg _ = todo ()
  end

  let run ~precolored ~register_order interference =
    let open Result.Let_syntax in
    let%bind color_of_name, alloc_of_color, used_registers =
      alloc_colors ~precolored ~register_order interference
    in
    let alloc_of_name = NameMap.create ~size:(Entity.Map.size color_of_name) () in
    Entity.Map.iteri color_of_name ~f:(fun (name, color) ->
      let alloc = ColorMap.find_exn alloc_of_color color in
      NameMap.set alloc_of_name ~key:name ~data:alloc;
      ());
    Ok { Allocation.alloc_of_name; used_registers }
  ;;
end

let%test_module _ =
  (module struct
    module Register = struct
      type t =
        | R1
        | R2
        | R3
        | R4
      [@@deriving equal, compare, hash, sexp]
    end

    module RegisterSet = struct
      type t = Register.t Hash_set.t [@@deriving sexp_of]
      type value = Register.t

      let create () = Hash_set.create (module Register)
      let mem = Hash_set.mem
      let add = Hash_set.add
    end

    module Reg_alloc = Make (Register) (RegisterSet)

    let _ = ()
  end)
;;
