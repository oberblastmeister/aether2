open! O
module Lir = Types

let make_lir s =
  lazy (s |> Parse.parse |> Or_error.ok_exn |> Elaborate.elaborate |> Or_error.ok_exn)
;;

let%test_module _ =
  (module struct
    let loop_lir =
      make_lir
        {|
(define (pow (b u64) (e u64)) u64
  (label (start)
    (set r (const u64 1))
    (jump (loop))
    )
  (label (loop)
    (set z (const u64 0))
    (set f (cmp u64 gt e z))
    (cond_jump f (done) (body))
    )
  (label (body)
    (set r (add u64 r b))
    (set one (const u64 1))
    (set e (add u64 e one))
    (jump (loop))
    )
  (label (done)
    (ret r)
    )
  )
|}
    ;;

    let%expect_test "uses" =
      let fn = List.hd_exn (Lazy.force loop_lir).functions in
      let p =
        Lir.(
          F.Fold.of_fn Function.graph
          @> F.Fold.of_fn Cfg.Graph.blocks
          @> Map.iter
          @> Block.instrs_forward_fold
          @> Some_instr.uses_fold)
      in
      let uses = F.Fold.reduce p F.Reduce.to_list_rev fn in
      print_s [%sexp (uses : Lir.Value.t list)];
      [%expect
        {|
    (((name f.4) (ty U1)) ((name z.3) (ty U64)) ((name e.1) (ty U64))
     ((name r.2) (ty U64)) ((name one.5) (ty U64)) ((name e.1) (ty U64))
     ((name b.0) (ty U64)) ((name r.2) (ty U64))) |}]
    ;;

    let%expect_test "liveness" =
      let fn = List.hd_exn (Lazy.force loop_lir).functions in
      let res, _ = Vir.Liveness.run fn.graph in
      print_s [%sexp (res : Lir.Value.Set.t Cfg.Dataflow.Fact_base.t)];
      ();
      [%expect
        {|
    ((start.0 (((name b.0) (ty U64)) ((name e.1) (ty U64))))
     (loop.1 (((name b.0) (ty U64)) ((name e.1) (ty U64)) ((name r.2) (ty U64))))
     (done.2 (((name r.2) (ty U64))))
     (body.3 (((name b.0) (ty U64)) ((name e.1) (ty U64)) ((name r.2) (ty U64))))) |}]
    ;;

    (* let%expect_test "dominators" =
       let fn = List.hd_exn (Lazy.force loop_lir).functions in
       let res, _ = Vir.DataflowDominators.run fn.graph in
       print_s [%sexp (res : Lir.Label.Set.t Cfg.Dataflow.Fact_base.t)];
       [%expect
        {|
    ((((name body) (id 3))
      (((name body) (id 3)) ((name loop) (id 1)) ((name start) (id 0))))
     (((name done) (id 2))
      (((name done) (id 2)) ((name loop) (id 1)) ((name start) (id 0))))
     (((name loop) (id 1)) (((name loop) (id 1)) ((name start) (id 0))))
     (((name start) (id 0)) (((name start) (id 0))))) |}]
       ;; *)

    (* let%expect_test "idoms" =
       let fn = List.hd_exn (Lazy.force loop_lir).functions in
       let dominators, _ = Vir.DataflowDominators.run fn.graph in
       let idoms =
       dominators |> Cfg.Dataflow.Dominators.compute_idoms_from_facts fn.graph.entry
       in
       let idom_tree =
       dominators |> Cfg.Dataflow.Dominators.compute_idom_tree_from_facts fn.graph.entry
       in
       print_s [%sexp "idoms", (idoms : Lir.Label.t Lir.Label.Map.t)];
       print_s [%sexp "idom_tree", (idom_tree : Lir.Label.Set.t Lir.Label.Map.t)];
       [%expect
        {|
    (idoms
     ((((name body) (id 3)) ((name loop) (id 1)))
      (((name done) (id 2)) ((name loop) (id 1)))
      (((name loop) (id 1)) ((name start) (id 0)))
      (((name start) (id 0)) ((name start) (id 0)))))
    (idom_tree
     ((((name loop) (id 1)) (((name body) (id 3)) ((name done) (id 2))))
      (((name start) (id 0)) (((name loop) (id 1)))))) |}]
       ;; *)

    let%expect_test "idoms fast" =
      let fn = List.hd_exn (Lazy.force loop_lir).functions in
      let idoms = Lir.Graph.get_idoms fn.graph in
      print_s [%sexp (idoms : Cfg.Dominators.Idoms.t)];
      ();
      [%expect
        {|
    ((start.0 start.0) (loop.1 start.0) (done.2 loop.1) (body.3 loop.1)) |}]
    ;;

    let%expect_test "naive ssa" =
      let program = Lazy.force loop_lir in
      let res =
        (Field.map Lir.Program.Fields.functions & List.map)
          ~f:Ssa.convert_naive_ssa
          program
      in
      print_endline @@ Pretty.pretty res;
      [%expect
        {|
    (define (pow (b.0 u64) (e.1 u64)) u64
      (label (start.0)
        (set r.2 (const u64 1))
        (jump (loop.1 b.0 e.1 r.2)))
      (label (loop.1 (b.9 u64) (e.10 u64) (r.11 u64))
        (set z.12 (const u64 0))
        (set f.13 (cmp u64 gt e.10 z.12))
        (cond_jump f.13 (done.2 r.11) (body.3 b.9 e.10 r.11)))
      (label (body.3 (b.3 u64) (e.4 u64) (r.5 u64))
        (set r.6 (add u64 r.5 b.3))
        (set one.7 (const u64 1))
        (set e.8 (add u64 e.4 one.7))
        (jump (loop.1 b.3 e.8 r.6)))
      (label (done.2 (r.14 u64))
        (ret r.14))) |}]
    ;;

    let%expect_test "ssa" =
      let program = Lazy.force loop_lir |> Ssa.convert_ssa in
      print_endline @@ Pretty.pretty program;
      [%expect
        {|
    (define (pow (b.0 u64) (e.1 u64)) u64
      (label (start.0)
        (set r.2 (const u64 1))
        (jump (loop.1 e.1 r.2)))
      (label (loop.1 (e.10 u64) (r.11 u64))
        (set z.12 (const u64 0))
        (set f.13 (cmp u64 gt e.10 z.12))
        (cond_jump f.13 (done.2) (body.3)))
      (label (body.3)
        (set r.6 (add u64 r.11 b.0))
        (set one.7 (const u64 1))
        (set e.8 (add u64 e.10 one.7))
        (jump (loop.1 e.8 r.6)))
      (label (done.2)
        (ret r.11))) |}]
    ;;

    let%expect_test "lower" =
      let program = Lazy.force loop_lir |> Ssa.convert_ssa |> Lower.run in
      print_endline @@ Lower.Tir.pretty program;
      [%expect
        {|
    (define (pow (b.0 u64) (e.1 u64)) u64
      (label (start.0)
        (jump (loop.1 e.1 (const u64 1))))
      (label (loop.1 (e.10 u64) (r.11 u64))
        (cond_jump (cmp u64 gt e.10 (const u64 0)) (done.2) (body.3)))
      (label (body.3)
        (jump (loop.1 (add u64 e.10 (const u64 1)) (add u64 r.11 b.0))))
      (label (done.2)
        (ret r.11))) |}]
    ;;

    let%expect_test "lower x86" =
      let program =
        Lazy.force loop_lir |> Ssa.convert_ssa |> Lower.run |> Lir_x86.lower
      in
      print_s @@ [%sexp_of: X86.Types.VReg.t X86.Types.Program.t] program;
      [%expect
        {|
        ((functions
          (((graph
             ((entry start.0)
              (blocks
               ((body.3
                 ((instrs
                   ((Block_args ()) (MovAbs (dst (Reg ((s Q) (name one.7)))) (imm 1))
                    (Add (dst (Reg ((s Q) (name e.8))))
                     (src1 (Reg ((s Q) (name e.10))))
                     (src2 (Reg ((s Q) (name one.7)))))
                    (Add (dst (Reg ((s Q) (name r.6))))
                     (src1 (Reg ((s Q) (name r.11))))
                     (src2 (Reg ((s Q) (name b.0)))))
                    (Jump
                     ((label loop.1) (args (((s Q) (name e.8)) ((s Q) (name r.6))))))))))
                (done.2
                 ((instrs ((Block_args ()) (Ret ((Reg ((s Q) (name r.11)))))))))
                (loop.1
                 ((instrs
                   ((Block_args (((s Q) (name e.10)) ((s Q) (name r.11))))
                    (MovAbs (dst (Reg ((s Q) (name z.12)))) (imm 0))
                    (Cmp (src1 (Reg ((s Q) (name e.10))))
                     (src2 (Reg ((s Q) (name z.12)))))
                    (Set (cond A) (dst (Reg ((s Q) (name f.13)))))
                    (Test (src1 (Reg ((s Q) (name f.13))))
                     (src2 (Reg ((s Q) (name f.13)))))
                    (CondJump (cond NE) (j1 ((label done.2) (args ())))
                     (j2 ((label body.3) (args ()))))))))
                (start.0
                 ((instrs
                   ((Block_args ()) (MovAbs (dst (Reg ((s Q) (name r.2)))) (imm 1))
                    (Jump
                     ((label loop.1) (args (((s Q) (name e.1)) ((s Q) (name r.2))))))))))))
              (exit done.2)))
            (unique_name 15) (caller_saved (RAX RDI RSI RDX RCX R8 R9 R10 R11))
            (stack_end_size 0))))) |}]
    ;;

    let%expect_test "regalloc" =
      let program =
        Lazy.force loop_lir
        |> Ssa.convert_ssa
        |> Lower.run
        |> Lir_x86.lower
        |> X86.Reg_alloc.run
      in
      print_s @@ [%sexp_of: X86.Types.MReg.t X86.Flat.Program.t] program;
      [%expect
        {|
        ((Label start.0) (Instr (MovAbs (dst (Reg (r R8))) (imm 1)))
         (Instr (Mov (dst (Reg (e RSI))) (src (Reg (e RAX)))))
         (Instr (Mov (dst (Reg (r RSI))) (src (Reg (r R8)))))
         (Instr (Jmp (src loop.1))) (Label loop.1)
         (Instr (MovAbs (dst (Reg (z R10))) (imm 0)))
         (Instr (Cmp (src1 (Reg (e RSI))) (src2 (Reg (z R10)))))
         (Instr (Set (cond A) (dst (Reg (f RDX)))))
         (Instr (Test (src1 (Reg (f RDX))) (src2 (Reg (f RDX)))))
         (Instr (J (cond NE) (src done.2))) (Instr (Jmp (src body.3))) (Label body.3)
         (Instr (MovAbs (dst (Reg (one RCX))) (imm 1)))
         (Instr (Mov (dst (Reg (e RDI))) (src (Reg (e RSI)))))
         (Instr (Add (dst (Reg (e RDI))) (src (Reg (one RCX)))))
         (Instr (Mov (dst (Reg (r R9))) (src (Reg (r RSI)))))
         (Instr (Add (dst (Reg (r R9))) (src (Reg (b RAX)))))
         (Instr (Mov (dst (Reg (e RSI))) (src (Reg (e RDI)))))
         (Instr (Mov (dst (Reg (r RSI))) (src (Reg (r R9)))))
         (Instr (Jmp (src loop.1))) (Label done.2)
         (Instr (Mov (dst (Reg RAX)) (src (Reg (r RSI)))))) |}]
    ;;
  end)
;;

let%test_module _ =
  (module struct
    let if_lir =
      make_lir
        {|
  (define (if (x u64) (y u64)) u64
    (label (start)
      (set one (const u64 9))
      (set f (cmp u64 gt x one))
      (cond_jump f (then) (else)))
    (label (then)
      (set r (const u64 3))
      (set r (add u64 r x))
      (jump (done)))
    (label (else)
      (set r (const u64 5))
      (set a (add u64 x y))
      (set r (add u64 r a))
      (jump (done)))
    (label (done)
      (ret r))
  )
  |}
    ;;

    let%expect_test "ssa" =
      let program = Lazy.force if_lir |> Ssa.convert_ssa in
      print_endline @@ Pretty.pretty program;
      ();
      [%expect
        {|
        (define (if (x.0 u64) (y.1 u64)) u64
          (label (start.0)
            (set one.2 (const u64 9))
            (set f.3 (cmp u64 gt x.0 one.2))
            (cond_jump f.3 (then.4) (else.5)))
          (label (else.5)
            (set r.6 (const u64 5))
            (set a.7 (add u64 x.0 y.1))
            (set r.8 (add u64 r.6 a.7))
            (jump (done.2 r.8)))
          (label (then.4)
            (set r.10 (const u64 3))
            (set r.11 (add u64 r.10 x.0))
            (jump (done.2 r.11)))
          (label (done.2 (r.12 u64))
            (ret r.12))) |}]
    ;;

    let%expect_test "lower" =
      let program = Lazy.force if_lir |> Ssa.convert_ssa |> Lower.run in
      print_endline @@ Lower.Tir.pretty program;
      [%expect
        {|
        (define (if (x.0 u64) (y.1 u64)) u64
          (label (start.0)
            (cond_jump (cmp u64 gt x.0 (const u64 9)) (then.4) (else.5)))
          (label (else.5)
            (jump (done.2 (add u64 (const u64 5) (add u64 x.0 y.1)))))
          (label (then.4)
            (jump (done.2 (add u64 (const u64 3) x.0))))
          (label (done.2 (r.12 u64))
            (ret r.12))) |}]
    ;;
  end)
;;
