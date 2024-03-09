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
          @> Block.iter_instrs_forward
          @> Some_instr.iter_uses)
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
      let program = Lazy.force loop_lir in
      let fn = List.hd_exn program.functions in
      let live_in, live_out = Vir.Liveness.run fn.graph in
      print_s [%sexp (live_in : Lir.Value.Set.t Cfg.Dataflow.Fact_base.t)];
      [%expect
        {|
    ((start.0 (((name b.0) (ty U64)) ((name e.1) (ty U64))))
     (loop.1 (((name b.0) (ty U64)) ((name e.1) (ty U64)) ((name r.2) (ty U64))))
     (done.2 (((name r.2) (ty U64))))
     (body.3 (((name b.0) (ty U64)) ((name e.1) (ty U64)) ((name r.2) (ty U64))))) |}];
      print_s [%sexp (live_out : Lir.Value.Set.t Cfg.Dataflow.Fact_base.t)];
      [%expect
        {|
        ((start.0
          (((name b.0) (ty U64)) ((name e.1) (ty U64)) ((name r.2) (ty U64))))
         (loop.1 (((name b.0) (ty U64)) ((name e.1) (ty U64)) ((name r.2) (ty U64))))
         (done.2 ())
         (body.3 (((name b.0) (ty U64)) ((name e.1) (ty U64)) ((name r.2) (ty U64))))) |}]
    ;;

    let%expect_test "liveness ssa" =
      let program = Lazy.force loop_lir |> Ssa.convert_ssa in
      let fn = List.hd_exn program.functions in
      let live_in, live_out = Vir.Liveness.run fn.graph in
      print_s [%sexp (live_in : Lir.Value.Set.t Cfg.Dataflow.Fact_base.t)];
      [%expect
        {|
    ((start.0 (((name b.0) (ty U64)) ((name e.1) (ty U64))))
     (loop.1 (((name b.0) (ty U64)))) (done.2 (((name r.11) (ty U64))))
     (body.3
      (((name b.0) (ty U64)) ((name e.10) (ty U64)) ((name r.11) (ty U64))))) |}];
      print_s [%sexp (live_out : Lir.Value.Set.t Cfg.Dataflow.Fact_base.t)];
      [%expect
        {|
        ((start.0 (((name b.0) (ty U64))))
         (loop.1
          (((name b.0) (ty U64)) ((name e.10) (ty U64)) ((name r.11) (ty U64))))
         (done.2 ()) (body.3 (((name b.0) (ty U64))))) |}]
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
          (((name pow)
            (graph
             ((entry start.0)
              (blocks
               ((body.3
                 ((instrs
                   ((Block_args ())
                    (Add (dst (Reg ((s Q) (name e.8))))
                     (src1 (Reg ((s Q) (name e.10)))) (src2 (Imm (Int 1))))
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
                    (Cmp (src1 (Reg ((s Q) (name e.10)))) (src2 (Imm (Int 0))))
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
            (params ((((s Q) (name b.0)) RDI) (((s Q) (name e.1)) RSI)))
            (stack_params ()) (unique_name 15) (unique_stack_slot 0)
            (caller_saved (RAX RDI RSI RDX RCX R8 R9 R10 R11)) (stack_instrs ()))))) |}]
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
        (SectionText (Type pow @function) (Global pow) (Label pow)
         (Instr (Sub (dst (Reg RSP)) (src (Imm (Int 8)))))
         (Instr (Mov (dst (Reg (b RAX))) (src (Reg RDI))))
         (Instr (Mov (dst (Reg (e RDI))) (src (Reg RSI)))) (Label .Lstart.0)
         (Instr (MovAbs (dst (Reg (r RSI))) (imm 1)))
         (Instr (Mov (dst (Reg (r RDX))) (src (Reg (r RSI)))))
         (Instr (Jmp (src .Lloop.1))) (Label .Lloop.1)
         (Instr (Cmp (src1 (Reg (e RDI))) (src2 (Imm (Int 0)))))
         (Instr (Set (cond A) (dst (Reg (f RSI)))))
         (Instr (Test (src1 (Reg (f RSI))) (src2 (Reg (f RSI)))))
         (Instr (J (cond NE) (src .Ldone.2))) (Instr (Jmp (src .Lbody.3)))
         (Label .Lbody.3) (Instr (Mov (dst (Reg (e RDI))) (src (Reg (e RDI)))))
         (Instr (Add (dst (Reg (e RDI))) (src (Imm (Int 1)))))
         (Instr (Mov (dst (Reg (r RSI))) (src (Reg (r RDX)))))
         (Instr (Add (dst (Reg (r RSI))) (src (Reg (b RAX)))))
         (Instr (Mov (dst (Reg (r RDX))) (src (Reg (r RSI)))))
         (Instr (Jmp (src .Lloop.1))) (Label .Ldone.2)
         (Instr (Mov (dst (Reg RAX)) (src (Reg (r RDX)))))
         (Instr (Add (dst (Reg RSP)) (src (Imm (Int 8))))) (Instr Ret)) |}]
    ;;

    let%expect_test "print" =
      let program =
        Lazy.force loop_lir
        |> Ssa.convert_ssa
        |> Lower.run
        |> Lir_x86.lower
        |> X86.Reg_alloc.run
        |> X86.Print.run
      in
      print_string program;
      [%expect
        {|
        	.text
        	.type	pow,@function
        	.globl	pow
        pow:
        	subq	rsp, 8
        	movq	rax, rdi
        	movq	rdi, rsi
        .Lstart.0:
        	movabsq	rsi, 1
        	movq	rdx, rsi
        	jmp .Lloop.1
        .Lloop.1:
        	cmpq	rdi, 0
        	seta	rsi
        	testq	rsi, rsi
        	jne .Ldone.2
        	jmp .Lbody.3
        .Lbody.3:
        	movq	rdi, rdi
        	addq	rdi, 1
        	movq	rsi, rdx
        	addq	rsi, rax
        	movq	rdx, rsi
        	jmp .Lloop.1
        .Ldone.2:
        	movq	rax, rdx
        	addq	rsp, 8
        	ret |}]
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

    let%expect_test "regalloc" =
      let program =
        Lazy.force if_lir
        |> Ssa.convert_ssa
        |> Lower.run
        |> Lir_x86.lower
        |> X86.Reg_alloc.run
      in
      print_s @@ [%sexp_of: X86.Types.MReg.t X86.Flat.Program.t] program;
      [%expect
        {|
        (SectionText (Type if @function) (Global if) (Label if)
         (Instr (Sub (dst (Reg RSP)) (src (Imm (Int 8)))))
         (Instr (Mov (dst (Reg (x RAX))) (src (Reg RDI)))) (Label .Lstart.0)
         (Instr (Cmp (src1 (Reg (x RAX))) (src2 (Imm (Int 9)))))
         (Instr (Set (cond A) (dst (Reg (f RDI)))))
         (Instr (Test (src1 (Reg (f RDI))) (src2 (Reg (f RDI)))))
         (Instr (J (cond NE) (src .Lthen.4))) (Instr (Jmp (src .Lelse.5)))
         (Label .Lelse.5) (Instr (Mov (dst (Reg (a RAX))) (src (Reg (x RAX)))))
         (Instr (Add (dst (Reg (a RAX))) (src (Reg (y RSI)))))
         (Instr (Mov (dst (Reg (r RAX))) (src (Imm (Int 5)))))
         (Instr (Add (dst (Reg (r RAX))) (src (Reg (a RAX)))))
         (Instr (Jmp (src .Ldone.2))) (Label .Lthen.4)
         (Instr (Mov (dst (Reg (r RAX))) (src (Imm (Int 3)))))
         (Instr (Add (dst (Reg (r RAX))) (src (Reg (x RAX)))))
         (Instr (Jmp (src .Ldone.2))) (Label .Ldone.2)
         (Instr (Mov (dst (Reg RAX)) (src (Reg (r RAX)))))
         (Instr (Add (dst (Reg RSP)) (src (Imm (Int 8))))) (Instr Ret)) |}]
    ;;

    let%expect_test "print" =
      let program =
        Lazy.force if_lir
        |> Ssa.convert_ssa
        |> Lower.run
        |> Lir_x86.lower
        |> X86.Reg_alloc.run
        |> X86.Print.run
      in
      print_string program;
      [%expect
        {|
        	.text
        	.type	if,@function
        	.globl	if
        if:
        	subq	rsp, 8
        	movq	rax, rdi
        .Lstart.0:
        	cmpq	rax, 9
        	seta	rdi
        	testq	rdi, rdi
        	jne .Lthen.4
        	jmp .Lelse.5
        .Lelse.5:
        	movq	rax, rax
        	addq	rax, rsi
        	movq	rax, 5
        	addq	rax, rax
        	jmp .Ldone.2
        .Lthen.4:
        	movq	rax, 3
        	addq	rax, rax
        	jmp .Ldone.2
        .Ldone.2:
        	movq	rax, rax
        	addq	rsp, 8
        	ret |}]
    ;;
  end)
;;

let%test_module _ =
  (module struct
    let fn_lir =
      make_lir
        {|
  (define (another (x u64) (y u64)) u64
    (label (start)
      (set bruh (const u64 1))
      (ret x))
    )

  (define (fn (x u64) (y u64)) u64
    (label (start)
      (set bruh (add u64 x y))
      (set res (call u64 (another x y)))
      (ret)))
    |}
    ;;

    let%expect_test _ =
      Logger.Log.with_log false (fun () ->
        let program =
          Lazy.force fn_lir
          |> Ssa.convert_ssa
          |> Lower.run
          |> Lir_x86.lower
          |> X86.Reg_alloc.run
          |> X86.Print.run
        in
        print_string program;
        [%expect
          {|
        	.text
        	.type	another,@function
        	.globl	another
        another:
        	subq	rsp, 8
        	movq	rax, rdi
        	movq	rdi, rsi
        .Lstart.0:
        	movabsq	rdi, 1
        	movq	rax, rax
        	addq	rsp, 8
        	ret
        	.type	fn,@function
        	.globl	fn
        fn:
        	subq	rsp, 8
        	movq	rax, rdi
        .Lstart.0:
        	movq	rdi, rax
        	addq	rdi, rsi
        	movq	rdi, rax
        	call	another
        	movq	rax, rax
        	addq	rsp, 8
        	ret |}])
    ;;
  end)
;;
