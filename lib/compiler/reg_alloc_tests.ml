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
    module Intern_table = Utils.Intern_table.String_intern

    let tbl = Intern_table.create ()
    let name = Intern_table.name_of_key tbl
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
      let allocation = Greedy.run ~precolored:[] ~interference:i in
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
      let allocation = Greedy.run ~precolored:[] ~interference:i in
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
      let allocation = Greedy.run ~precolored:[] ~interference:i in
      print_s @@ [%sexp (allocation : Allocation.t)];
      [%expect
        {|
        ((alloc_of_name
          ((b.0 (InReg R1)) (d.1 (InReg R3)) (a.2 (InReg R2)) (c.3 (InReg R2))))
         (used_registers (R1 R2 R3))) |}]
    ;;
  end)
;;
