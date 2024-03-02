open O
open Utils.Instr_types

let%test_module _ =
  (module struct
    module Register = struct
      type t =
        | R1
        | R2
        | R3
        | R4
      [@@deriving equal, compare, hash, sexp, enum]
    end

    module Ra = Reg_alloc.Make (struct
        module Register = Register

        let config = { Reg_alloc.Types.register_order = Register.[ R1; R2; R3; R4 ] }
      end)

    open Ra

    (* let enum =
      let module M = Data.Enum_set.Make_enum (Register) in
      M.enum
    ;;

    let dict =
      { config
      ; register = { enum; sexp_of = Register.sexp_of_t; equal = Register.equal }
      }
    ;; *)

    (* module Config = struct
       module Register = Register
       module RegisterSet = RegisterSet
       end *)

    (* module Reg_alloc = Make (Config)
       module Allocation = Make_allocation (Config)
       module Constraints = Reg_constraints.Make (Config) *)
    module Intern_table = Entity.Intern_table

    let tbl = Intern_table.create (module Name)
    let name = Intern_table.name_of_string tbl
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
        Greedy.run
          ~precolored:(Entity.Map.create ())
          ~interference:i
          ~constraints:(Constraints.create ())
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
        Greedy.run ~precolored ~interference:i ~constraints:(Constraints.create ())
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
        Greedy.run ~precolored ~interference:i ~constraints:(Constraints.create ())
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