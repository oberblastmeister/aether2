open O
open Utils.Instr_types
module NameMap = Entity.Map.Make (Name)
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

(* TODO: do greedy coalescing *)
module Make (Arg : Arg) = struct
  open Arg
  open Config

  type error = InvalidRegisterConstraint of Register.t * Register.t

  let simplicial_elimination_ordering ~interference ~precolored =
    let heap = IntHeap.create ~size:(Interference.size interference) () in
    interference
    |> Interference.nodes
    |> F.Iter.filter ~f:(fun node -> not @@ NameMap.mem precolored node)
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
    let color_of_name = NameMap.create () in
    let max_color = ref Color.lowest in
    let used_colors = Hash_set.create (module Color) in
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
      NameMap.set color_of_name ~key:name ~data:lowest_not_in_neighbors;
      max_color := Color.max !max_color lowest_not_in_neighbors;
      Hash_set.add used_colors lowest_not_in_neighbors;
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
        Or_error.error_s
          [%message "invalid register constraint" (reg : Register.t) (reg' : Register.t)]
      | _ -> Ok ()
    in
    Ok register_constraints_of_color
  ;;

  let color ~interference ~precolored =
    color_with
      ~interference
      ~ordering:(simplicial_elimination_ordering ~interference ~precolored)
  ;;

  let alloc_colors ~precolored ~interference =
    let open Result.Let_syntax in
    let used_registers = RegisterSet.create () in
    let color_of_name, max_color = color ~interference ~precolored in
    let%bind register_constraints_of_color =
      collect_precolored_constraints ~interference ~color_of_name ~precolored
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
            of_list Register.order
            |> F.Iter.find_pred ~f:(fun reg ->
              Option.value_map
                register_constraints
                ~default:true
                ~f:(fun register_constraints ->
                  not (RegisterSet.mem register_constraints reg))
              && not (RegisterSet.mem used_registers reg)))
        in
        let alloc_reg =
          reg |> Option.value_map ~default:Alloc_reg.Spilled ~f:Alloc_reg.inreg
        in
        Option.iter reg ~f:(RegisterSet.add used_registers);
        ColorMap.set alloc_of_color ~key:color ~data:alloc_reg;
        ()));
    Ok (color_of_name, alloc_of_color, used_registers)
  ;;

  let run ~precolored ~interference =
    let open Result.Let_syntax in
    let%bind color_of_name, alloc_of_color, used_registers =
      alloc_colors ~precolored ~interference
    in
    let alloc_of_name =
      Entity.Map.to_iteri color_of_name
      |> F.Iter.map ~f:(fun (name, color) ->
        let alloc = ColorMap.find_exn alloc_of_color color in
        name, alloc)
      |> NameMap.of_iter ~size:(Interference.size interference)
    in
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
      [@@deriving equal, compare, hash, sexp, enum]

      let order = [ R1; R2; R3; R4 ]
    end

    module RegisterSet = Data.Enum_set.Make (Register)

    module Arg = Make_arg (struct
        module Register = Register
        module RegisterSet = RegisterSet
      end)

    module Reg_alloc = Make (Arg)
    module Allocation = Arg.Allocation

    let tbl = Hashtbl.create (module String)

    let name s =
      match Hashtbl.find tbl s with
      | None ->
        let label = Name.of_string_global_unique s in
        Hashtbl.set tbl ~key:s ~data:label;
        label
      | Some l -> l
    ;;

    let b = name "b"
    let d = name "d"
    let a = name "a"
    let c = name "c"

    let%expect_test "no interfer" =
      let i = Interference.create () in
      Interference.add_node i a;
      Interference.add_node i b;
      Interference.add_node i c;
      Interference.add_node i d;
      let allocation =
        Reg_alloc.run ~precolored:(Entity.Map.create ()) ~interference:i
        |> Result.map_error ~f:(fun _ -> "wrong")
        |> Result.ok_or_failwith
      in
      print_s @@ [%sexp (allocation : Allocation.t)];
      ();
      [%expect
        {|
        ((alloc_of_name
          ((b.0 (InReg R1)) (d.1 (InReg R1)) (a.2 (InReg R1)) (c.3 (InReg R1))))
         (used_registers (R1))) |}]
    ;;

    let%expect_test "simple interfere" =
      let module I = Interference in
      let i = I.create () in
      I.add_node i a;
      I.add_node i b;
      I.add_node i c;
      I.add_node i d;
      I.add_edge i b a;
      I.add_edge i b c;
      I.add_edge i b d;
      let precolored = Entity.Map.create () in
      let allocation =
        Reg_alloc.run ~precolored ~interference:i
        |> Result.map_error ~f:(fun _ -> "wrong")
        |> Result.ok_or_failwith
      in
      print_s @@ [%sexp (allocation : Allocation.t)];
      [%expect
        {|
        ((alloc_of_name
          ((b.0 (InReg R1)) (d.1 (InReg R2)) (a.2 (InReg R2)) (c.3 (InReg R2))))
         (used_registers (R1 R2))) |}];
      ()
    ;;

    let%expect_test "simple precolored" =
      let module I = Interference in
      let i = I.create () in
      I.add_node i a;
      I.add_node i b;
      I.add_node i c;
      I.add_node i d;
      I.add_edge i b a;
      I.add_edge i b c;
      I.add_edge i b d;
      I.add_edge i a d;
      let precolored = Entity.Map.create () in
      let allocation =
        Reg_alloc.run ~precolored ~interference:i
        |> Result.map_error ~f:(fun _ -> "wrong")
        |> Result.ok_or_failwith
      in
      print_s @@ [%sexp (allocation : Allocation.t)];
      [%expect
        {|
        ((alloc_of_name
          ((b.0 (InReg R1)) (d.1 (InReg R3)) (a.2 (InReg R2)) (c.3 (InReg R2))))
         (used_registers (R1 R2 R3))) |}]
    ;;
  end)
;;
