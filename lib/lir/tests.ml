open! O
module Lir = Ast

let parse s = Driver.parse_string s |> Or_error.ok_exn
let make_lir s = lazy (parse s)

let%test_module _ =
  (module struct
    let loop_lir =
      make_lir
        {|
(define (pow [b u64] [e u64]) u64
  (block (start)
    (set r (const u64 1))
    (jump (loop))
    )
  (block (loop)
    (set z (const u64 0))
    (set f (cmp u64 gt e z))
    (cond_jump f (done) (body))
    )
  (block (body)
    (set r (add u64 r b))
    (set one (const u64 1))
    (set e (add u64 e one))
    (jump (loop))
    )
  (block (done)
    (ret r)
    )
  )
|}
    ;;

    let%expect_test "uses" =
      let fn = List.hd_exn (Lazy.force loop_lir).funcs in
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
      let fn = List.hd_exn program.funcs in
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
      let program = Lazy.force loop_lir |> Ssa.convert in
      let fn = List.hd_exn program.funcs in
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
      let fn = List.hd_exn (Lazy.force loop_lir).funcs in
      let idoms = Lir.Graph.get_idoms fn.graph in
      print_s [%sexp (idoms : Cfg.Dominators.Idoms.t)];
      ();
      [%expect {|
    ((loop.1 start.0) (done.2 loop.1) (body.3 loop.1)) |}]
    ;;

    let%expect_test "naive ssa" =
      let program = Lazy.force loop_lir in
      let res =
        (Field.map Lir.Program.Fields.funcs & List.map) ~f:Ssa.convert_naive program
      in
      print_endline @@ Pretty.pretty res;
      [%expect
        {|
    (define (pow [b.0 u64] [e.1 u64]) u64
      (block (start.0)
        (set r.2 (const u64 1))
        (jump (loop.1 b.0 e.1 r.2)))
      (block (loop.1 [b.9 u64] [e.10 u64] [r.11 u64])
        (set z.12 (const u64 0))
        (set f.13 (cmp u64 gt e.10 z.12))
        (cond_jump f.13 (done.2 r.11) (body.3 b.9 e.10 r.11)))
      (block (body.3 [b.3 u64] [e.4 u64] [r.5 u64])
        (set r.6 (add u64 r.5 b.3))
        (set one.7 (const u64 1))
        (set e.8 (add u64 e.4 one.7))
        (jump (loop.1 b.3 e.8 r.6)))
      (block (done.2 [r.14 u64])
        (ret r.14))) |}]
    ;;

    let%expect_test "ssa" =
      let program = Lazy.force loop_lir |> Ssa.convert in
      print_endline @@ Pretty.pretty program;
      [%expect
        {|
    (define (pow [b.0 u64] [e.1 u64]) u64
      (block (start.0)
        (set r.2 (const u64 1))
        (jump (loop.1 e.1 r.2)))
      (block (loop.1 [e.10 u64] [r.11 u64])
        (set z.12 (const u64 0))
        (set f.13 (cmp u64 gt e.10 z.12))
        (cond_jump f.13 (done.2) (body.3)))
      (block (body.3)
        (set r.6 (add u64 r.11 b.0))
        (set one.7 (const u64 1))
        (set e.8 (add u64 e.10 one.7))
        (jump (loop.1 e.8 r.6)))
      (block (done.2)
        (ret r.11))) |}]
    ;;

    let%expect_test "lower" =
      let program = Lazy.force loop_lir |> Ssa.convert |> Lower.run in
      print_endline @@ Lower.Tir.pretty program;
      [%expect
        {|
    (define (pow [b.0 u64] [e.1 u64]) u64
      (block (start.0)
        (jump (loop.1 e.1 (const u64 1))))
      (block (loop.1 [e.10 u64] [r.11 u64])
        (cond_jump (cmp u64 gt e.10 (const u64 0)) (done.2) (body.3)))
      (block (body.3)
        (jump (loop.1 (add u64 e.10 (const u64 1)) (add u64 r.11 b.0))))
      (block (done.2)
        (ret r.11))) |}]
    ;;

    let%expect_test "lower x86" =
      let program = Lazy.force loop_lir |> Ssa.convert |> Lower.run |> Lir_x86.lower in
      print_s @@ [%sexp_of: X86.Ast.VReg.t X86.Ast.Program.t] program;
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
                    (Mov (dst (Reg ((s Q) (name expr_tmp.16))))
                     (src (Reg ((s Q) (name e.10)))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.17)))) (src (Imm (Int 1))))
                    (Add (dst (Reg ((s Q) (name e.8))))
                     (src1 (Reg ((s Q) (name expr_tmp.16))))
                     (src2 (Reg ((s Q) (name expr_tmp.17)))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.15))))
                     (src (Reg ((s Q) (name e.8)))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.19))))
                     (src (Reg ((s Q) (name r.11)))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.20))))
                     (src (Reg ((s Q) (name b.0)))))
                    (Add (dst (Reg ((s Q) (name r.6))))
                     (src1 (Reg ((s Q) (name expr_tmp.19))))
                     (src2 (Reg ((s Q) (name expr_tmp.20)))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.18))))
                     (src (Reg ((s Q) (name r.6)))))
                    (Jump
                     ((label loop.1)
                      (args (((s Q) (name expr_tmp.15)) ((s Q) (name expr_tmp.18))))))))))
                (done.2
                 ((instrs
                   ((Block_args ())
                    (Mov (dst (Reg ((s Q) (name expr_tmp.21))))
                     (src (Reg ((s Q) (name r.11)))))
                    (Ret ((Reg ((s Q) (name expr_tmp.21)))))))))
                (loop.1
                 ((instrs
                   ((Block_args (((s Q) (name e.10)) ((s Q) (name r.11))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.23))))
                     (src (Reg ((s Q) (name e.10)))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.24)))) (src (Imm (Int 0))))
                    (Cmp (src1 (Reg ((s Q) (name expr_tmp.23))))
                     (src2 (Reg ((s Q) (name expr_tmp.24)))))
                    (Set (cond A) (dst (Reg ((s B) (name f.13)))))
                    (Mov (dst (Reg ((s B) (name expr_tmp.22))))
                     (src (Reg ((s B) (name f.13)))))
                    (Test (src1 (Reg ((s B) (name expr_tmp.22))))
                     (src2 (Reg ((s B) (name expr_tmp.22)))))
                    (CondJump (cond NE) (j1 ((label done.2) (args ())))
                     (j2 ((label body.3) (args ()))))))))
                (start.0
                 ((instrs
                   ((Block_args ())
                    (Mov (dst (Reg ((s Q) (name expr_tmp.25))))
                     (src (Reg ((s Q) (name e.1)))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.26)))) (src (Imm (Int 1))))
                    (Jump
                     ((label loop.1)
                      (args (((s Q) (name expr_tmp.25)) ((s Q) (name expr_tmp.26))))))))))))
              (exit done.2)))
            (params ((((s Q) (name b.0)) RDI) (((s Q) (name e.1)) RSI)))
            (stack_params ()) (unique_name 27) (unique_stack_slot 0)
            (caller_saved (RAX RDI RSI RDX RCX R8 R9 R10 R11)) (stack_instrs ()))))) |}]
    ;;

    let%expect_test "print" =
      let program =
        Lazy.force loop_lir
        |> Ssa.convert
        |> Lower.run
        |> Lir_x86.lower
        |> X86.Driver.compile_program
        |> X86.Print.run
      in
      print_string program;
      [%expect
        {|
        	.intel_syntax	noprefix
        	.text
        	.type	pow,@function
        	.globl	pow
        pow:
        	sub	rsp, 0
        	mov	rax, rdi
        	mov	rdi, rsi
        # label: start
        .L00:
        	mov	rdi, rdi
        	mov	rsi, 1
        	mov	rdx, rsi
        	jmp .L01
        # label: loop
        .L01:
        	mov	rdi, rdi
        	mov	rsi, 0
        	cmp	rdi, rsi
        	seta	sil
        	mov	sil, sil
        	test	sil, sil
        	jne .L02
        	jmp .L03
        # label: body
        .L03:
        	mov	rsi, rdi
        	mov	rdi, 1
        	mov	r11, rsi
        	add	r11, rdi
        	mov	rdi, r11
        	mov	rdi, rdi
        	mov	rsi, rdx
        	mov	rax, rax
        	mov	r11, rsi
        	add	r11, rax
        	mov	rsi, r11
        	mov	rsi, rsi
        	mov	rdx, rsi
        	jmp .L01
        # label: done
        .L02:
        	mov	rax, rdx
        	mov	rax, rax
        	add	rsp, 0
        	ret |}]
    ;;
  end)
;;

let%test_module _ =
  (module struct
    let if_lir =
      make_lir
        {|
  (define (if [x u64] [y u64]) u64
    (block (start)
      (set one (const u64 9))
      (set f (cmp u64 gt x one))
      (cond_jump f (then) (else)))
    (block (then)
      (set r (const u64 3))
      (set r (add u64 r x))
      (jump (done)))
    (block (else)
      (set r (const u64 5))
      (set a (add u64 x y))
      (set r (add u64 r a))
      (jump (done)))
    (block (done)
      (ret r))
  )
  |}
    ;;

    let%expect_test "ssa" =
      let program = Lazy.force if_lir |> Ssa.convert in
      print_endline @@ Pretty.pretty program;
      ();
      [%expect
        {|
        (define (if [x.0 u64] [y.1 u64]) u64
          (block (start.0)
            (set one.2 (const u64 9))
            (set f.3 (cmp u64 gt x.0 one.2))
            (cond_jump f.3 (then.1) (else.2)))
          (block (else.2)
            (set r.6 (const u64 5))
            (set a.7 (add u64 x.0 y.1))
            (set r.8 (add u64 r.6 a.7))
            (jump (done.3 r.8)))
          (block (then.1)
            (set r.10 (const u64 3))
            (set r.11 (add u64 r.10 x.0))
            (jump (done.3 r.11)))
          (block (done.3 [r.12 u64])
            (ret r.12))) |}]
    ;;

    let%expect_test "lower" =
      let program = Lazy.force if_lir |> Ssa.convert |> Lower.run in
      print_endline @@ Lower.Tir.pretty program;
      [%expect
        {|
        (define (if [x.0 u64] [y.1 u64]) u64
          (block (start.0)
            (cond_jump (cmp u64 gt x.0 (const u64 9)) (then.1) (else.2)))
          (block (else.2)
            (jump (done.3 (add u64 (const u64 5) (add u64 x.0 y.1)))))
          (block (then.1)
            (jump (done.3 (add u64 (const u64 3) x.0))))
          (block (done.3 [r.12 u64])
            (ret r.12))) |}]
    ;;

    let%expect_test "x86 lower" =
      let program = Lazy.force if_lir |> Ssa.convert |> Lower.run |> Lir_x86.lower in
      print_s @@ [%sexp_of: X86.Ast.VReg.t X86.Ast.Program.t] program;
      [%expect
        {|
        ((functions
          (((name if)
            (graph
             ((entry start.0)
              (blocks
               ((done.3
                 ((instrs
                   ((Block_args (((s Q) (name r.12))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.13))))
                     (src (Reg ((s Q) (name r.12)))))
                    (Ret ((Reg ((s Q) (name expr_tmp.13)))))))))
                (else.2
                 ((instrs
                   ((Block_args ())
                    (Mov (dst (Reg ((s Q) (name expr_tmp.15)))) (src (Imm (Int 5))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.17))))
                     (src (Reg ((s Q) (name x.0)))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.18))))
                     (src (Reg ((s Q) (name y.1)))))
                    (Add (dst (Reg ((s Q) (name a.7))))
                     (src1 (Reg ((s Q) (name expr_tmp.17))))
                     (src2 (Reg ((s Q) (name expr_tmp.18)))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.16))))
                     (src (Reg ((s Q) (name a.7)))))
                    (Add (dst (Reg ((s Q) (name r.8))))
                     (src1 (Reg ((s Q) (name expr_tmp.15))))
                     (src2 (Reg ((s Q) (name expr_tmp.16)))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.14))))
                     (src (Reg ((s Q) (name r.8)))))
                    (Jump ((label done.3) (args (((s Q) (name expr_tmp.14))))))))))
                (start.0
                 ((instrs
                   ((Block_args ())
                    (Mov (dst (Reg ((s Q) (name expr_tmp.20))))
                     (src (Reg ((s Q) (name x.0)))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.21)))) (src (Imm (Int 9))))
                    (Cmp (src1 (Reg ((s Q) (name expr_tmp.20))))
                     (src2 (Reg ((s Q) (name expr_tmp.21)))))
                    (Set (cond A) (dst (Reg ((s B) (name f.3)))))
                    (Mov (dst (Reg ((s B) (name expr_tmp.19))))
                     (src (Reg ((s B) (name f.3)))))
                    (Test (src1 (Reg ((s B) (name expr_tmp.19))))
                     (src2 (Reg ((s B) (name expr_tmp.19)))))
                    (CondJump (cond NE) (j1 ((label then.1) (args ())))
                     (j2 ((label else.2) (args ()))))))))
                (then.1
                 ((instrs
                   ((Block_args ())
                    (Mov (dst (Reg ((s Q) (name expr_tmp.23)))) (src (Imm (Int 3))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.24))))
                     (src (Reg ((s Q) (name x.0)))))
                    (Add (dst (Reg ((s Q) (name r.11))))
                     (src1 (Reg ((s Q) (name expr_tmp.23))))
                     (src2 (Reg ((s Q) (name expr_tmp.24)))))
                    (Mov (dst (Reg ((s Q) (name expr_tmp.22))))
                     (src (Reg ((s Q) (name r.11)))))
                    (Jump ((label done.3) (args (((s Q) (name expr_tmp.22))))))))))))
              (exit done.3)))
            (params ((((s Q) (name x.0)) RDI) (((s Q) (name y.1)) RSI)))
            (stack_params ()) (unique_name 25) (unique_stack_slot 0)
            (caller_saved (RAX RDI RSI RDX RCX R8 R9 R10 R11)) (stack_instrs ()))))) |}]
    ;;

    let%expect_test "liveness" =
      let@ () = Logger.with_log false in
      let program = Lazy.force if_lir |> Ssa.convert |> Lower.run |> Lir_x86.lower in
      let fn = List.hd_exn program.functions in
      let live_in, live_out = X86.Dataflow.Liveness.run fn in
      print_s
        [%message
          (live_in : X86.Ast.VReg.Set.t Cfg.Dataflow.Fact_base.t)
            (live_out : X86.Ast.VReg.Set.t Cfg.Dataflow.Fact_base.t)];
      [%expect
        {|
        ((live_in
          ((start.0 (((s Q) (name x.0)) ((s Q) (name y.1))))
           (then.1 (((s Q) (name x.0))))
           (else.2 (((s Q) (name x.0)) ((s Q) (name y.1)))) (done.3 ())))
         (live_out
          ((start.0 (((s Q) (name x.0)) ((s Q) (name y.1)))) (then.1 ()) (else.2 ())
           (done.3 ())))) |}]
    ;;

    let%expect_test "print" =
      let@ () = Logger.with_log false in
      let program =
        Lazy.force if_lir
        |> Ssa.convert
        |> Lower.run
        |> Lir_x86.lower
        |> X86.Driver.compile_program
        |> X86.Print.run
      in
      print_string program;
      [%expect
        {|
        	.intel_syntax	noprefix
        	.text
        	.type	if,@function
        	.globl	if
        if:
        	sub	rsp, 0
        	mov	rax, rdi
        # label: start
        .L00:
        	mov	rax, rax
        	mov	rdi, 9
        	cmp	rax, rdi
        	seta	dil
        	mov	dil, dil
        	test	dil, dil
        	jne .L01
        	jmp .L02
        # label: else
        .L02:
        	mov	rdi, 5
        	mov	rax, rax
        	mov	rsi, rsi
        	mov	r11, rax
        	add	r11, rsi
        	mov	rax, r11
        	mov	rax, rax
        	mov	r11, rdi
        	add	r11, rax
        	mov	rax, r11
        	mov	rax, rax
        	jmp .L03
        # label: then
        .L01:
        	mov	rdi, 3
        	mov	rax, rax
        	mov	r11, rdi
        	add	r11, rax
        	mov	rax, r11
        	mov	rax, rax
        	jmp .L03
        # label: done
        .L03:
        	mov	rax, rax
        	mov	rax, rax
        	add	rsp, 0
        	ret |}]
    ;;
  end)
;;

let%test_module _ =
  (module struct
    let fn_lir =
      make_lir
        {|
  (define (another [x u64] [y u64]) u64
    (block (start)
      (set bruh (const u64 1))
      (ret x))
    )

  (define (fn [x u64] [y u64]) void
    (block (start)
      (set bruh (add u64 x y))
      (set res (call u64 (another x y)))
      (ret)))
    |}
    ;;

    let%expect_test "simple function" =
      let@ () = Logger.with_log false in
      let program =
        Lazy.force fn_lir
        |> Ssa.convert
        |> Lower.run
        |> Lir_x86.lower
        |> X86.Driver.compile_program
        |> X86.Print.run
      in
      print_string program;
      [%expect
        {|
        	.intel_syntax	noprefix
        	.text
        	.type	another,@function
        	.globl	another
        another:
        	sub	rsp, 0
        	mov	rax, rdi
        	mov	rdi, rsi
        # label: start
        .L00:
        	mov	rdi, 1
        	mov	rax, rax
        	mov	rax, rax
        	add	rsp, 0
        	ret
        	.type	fn,@function
        	.globl	fn
        fn:
        	sub	rsp, 0
        	mov	rax, rdi
        # label: start
        .L10:
        	mov	rax, rax
        	mov	rdi, rsi
        	mov	r11, rax
        	add	r11, rdi
        	mov	rdi, r11
        	mov	rax, rax
        	mov	rdi, rsi
        	mov	rsi, rdi
        	mov	rdi, rax
        	call	another
        	mov	rax, rax
        	add	rsp, 0
        	ret |}]
    ;;
  end)
;;

let%expect_test "parse extern" =
  let lir =
    parse
      {|
  (extern (extern_function u64 u64) u64)
  
  (extern (another u64 u64) u64)
  
  (define (testing [x u64] [y u64]) void
    (block (start)
      (ret)))
  |}
  in
  print_s [%sexp (lir : Ast.Value.t Ast.Program.t)];
  [%expect
    {|
    ((funcs
      (((name testing)
        (graph
         ((entry start.0)
          (blocks ((start.0 ((entry ()) (body ()) (exit (Ret ()))))))
          (exit start.0)))
        (ty
         ((params (((name x.0) (ty U64)) ((name y.1) (ty U64)))) (return Void)))
        (unique_label 1) (unique_name 2))))
     (externs
      (((name extern_function) (ty ((params (U64 U64)) (return U64))))
       ((name another) (ty ((params (U64 U64)) (return U64))))))) |}]
;;

(* let%expect_test "type check" =
  let lir =
    parse
      {|
  (define (first [x u64] [y u64]) u64
    (block (start)
      (ret))
  )
  |}
  in
  ()
;; *)
