open O
open Utils.Instr_types
include Check_ssa_intf

module Make (Config : Config) = struct
  open Config

  let or_error_of_list = function
    | [] -> Ok ()
    | _ :: _ as es -> Error.of_list es |> Error
  ;;

  let check_all_temps_unique fn =
    let errors : Error.t Stack.t = Stack.create () in
    let defines = Hash_set.create (module Name) in
    let check_define label instr def =
      if Hash_set.mem defines def
      then
        Stack.push
          errors
          (Error.t_of_sexp
             [%message
               "a temporary was defined more than once"
                 ~label:(label : Label.t option)
                 ~instr:(instr : Instr.t option)
                 ~def:(def : Name.t)]);
      Hash_set.add defines def
    in
    List.iter (Function.params fn) ~f:(check_define None None);
    let iter =
      F.Fold.(
        Graph.iteri_blocks @> ix (dup Block.iter_instrs_forward) @> ix2 Instr.iter_defs)
    in
    F.Fold.iter
      iter
      ~f:(fun (label, (i, def)) -> check_define (Some label) (Some i) def)
      (Function.to_graph fn);
    Stack.to_list errors |> or_error_of_list
  ;;

  let check_dominators fn =
    let module Dominators = Cfg.Dominators in
    let errors : Error.t Stack.t = Stack.create () in
    let graph = Function.to_graph fn in
    let domtree =
      Dominators.get_idoms ~start:(Graph.start graph) (Graph.to_double_graph graph)
      |> Dominators.Domtree.of_idoms
    in
    let rec go scope label =
      let block = Graph.find_block_exn graph label in
      let scope =
        let scope = ref scope in
        Block.iter_instrs_forward block ~f:(fun instr ->
          Instr.iter_uses instr ~f:(fun use ->
            if not @@ Set.mem !scope use
            then (
              Stack.push
                errors
                (Error.t_of_sexp
                   [%message
                     "a use was not dominated by a define"
                       ~instr:(instr : Instr.t)
                       ~use:(use : Name.t)
                       ~label:(label : Label.t)
                       ~scope:(!scope : Name.Set.t)]);
              ());
            ());
          Instr.iter_defs instr ~f:(fun def -> scope := Set.add !scope def);
          ());
        !scope
      in
      let children = Dominators.Domtree.children domtree label in
      List.iter children ~f:(go scope);
      ()
    in
    let initial_scope = Function.params fn |> Name.Set.of_list in
    go initial_scope (Graph.start graph);
    Stack.to_list errors |> or_error_of_list
  ;;

  let check fn =
    let open Or_error.Let_syntax in
    let%bind () = check_all_temps_unique fn in
    let%bind () = check_dominators fn in
    return ()
  ;;

  let check_list list =
    let open Or_error.Let_syntax in
    let rec go = function
      | [] -> Ok ()
      | fn :: fns ->
        let%bind () = check fn in
        go fns
    in
    go list
  ;;
end
