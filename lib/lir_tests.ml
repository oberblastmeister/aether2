open O

let loop_lir =
  lazy
    ({|
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
     |> Lir.parse
     |> Or_error.ok_exn)
;;

let%expect_test "uses" =
  let fn = List.hd_exn (Lazy.force loop_lir).functions in
  let p =
    Lir.(
      G.Fold.of_fn Function.body
      @> G.Fold.of_fn Graph.blocks
      @> G.Core.Map.fold
      @> Block.instrs_forward_fold
      @> Instr.uses_fold)
  in
  let uses = G.Fold.reduce p G.Reduce.to_list_rev fn in
  print_s [%sexp (uses : Lir.Value.t list)];
  [%expect
    {|
    (((name (Name f)) (ty U64)) ((name (Name z)) (ty U64))
     ((name (Name e)) (ty U64)) ((name (Name r)) (ty U64))
     ((name (Name one)) (ty U64)) ((name (Name e)) (ty U64))
     ((name (Name b)) (ty U64)) ((name (Name r)) (ty U64))) |}]
;;

let%expect_test "liveness" =
  let fn = List.hd_exn (Lazy.force loop_lir).functions in
  let res = Lir.Liveness.run fn.body in
  print_s [%sexp (res : Lir.Liveness.InstrTransfer.domain Lir.Label.Map.t)];
  ();
  [%expect
    {|
    ((((name (Name body)))
      (((name (Name b)) (ty U64)) ((name (Name e)) (ty U64))
       ((name (Name r)) (ty U64))))
     (((name (Name done))) (((name (Name r)) (ty U64))))
     (((name (Name loop)))
      (((name (Name b)) (ty U64)) ((name (Name e)) (ty U64))
       ((name (Name r)) (ty U64))))
     (((name (Name start)))
      (((name (Name b)) (ty U64)) ((name (Name e)) (ty U64))))) |}]
;;

let%expect_test "dominators" =
  let fn = List.hd_exn (Lazy.force loop_lir).functions in
  let res = Lir.DataflowDominators.run fn.body in
  print_s [%sexp (res : Lir.DataflowDominators.BlockTransfer.domain Lir.Label.Map.t)];
  [%expect
    {|
    ((((name (Name body)))
      (((name (Name body))) ((name (Name loop))) ((name (Name start)))))
     (((name (Name done)))
      (((name (Name done))) ((name (Name loop))) ((name (Name start)))))
     (((name (Name loop))) (((name (Name loop))) ((name (Name start)))))
     (((name (Name start))) (((name (Name start)))))) |}]
;;

let%expect_test "idoms" =
  let fn = List.hd_exn (Lazy.force loop_lir).functions in
  let dominators = Lir.DataflowDominators.run fn.body in
  let idoms = dominators |> Cfg.Dominators.compute_idoms_from_facts fn.body.entry in
  let idom_tree =
    dominators |> Cfg.Dominators.compute_idom_tree_from_facts fn.body.entry
  in
  print_s [%sexp "idoms", (idoms : Lir.Label.t Lir.Label.Map.t)];
  print_s [%sexp "idom_tree", (idom_tree : Cfg.DominatorFact.t Lir.Label.Map.t)];
  [%expect {|
    (idoms
     ((((name (Name body))) ((name (Name loop))))
      (((name (Name done))) ((name (Name loop))))
      (((name (Name loop))) ((name (Name start))))
      (((name (Name start))) ((name (Name start))))))
    (idom_tree
     ((((name (Name loop))) (((name (Name body))) ((name (Name done)))))
      (((name (Name start))) (((name (Name loop))))))) |}]
;;

let%expect_test "idoms fast" =
  let fn = List.hd_exn (Lazy.force loop_lir).functions in
  let idoms = Lir.Dominators.get_idoms fn.body in
  print_s [%sexp (idoms : Lir.Label.t Lir.Label.Hashtbl.t)];
  ();
  [%expect {|
    ((((name (Name body))) ((name (Name loop))))
     (((name (Name done))) ((name (Name loop))))
     (((name (Name loop))) ((name (Name start))))
     (((name (Name start))) ((name (Name start))))) |}]

let%expect_test "naive ssa" =
  let program = Lazy.force loop_lir in
  let res =
    (Field.map Lir.Program.Fields.functions & List.map)
      ~f:Lir.Ssa.convert_naive_ssa
      program
  in
  print_endline @@ Lir.pretty res;
  [%expect
    {|
    (define (pow (b.0 u64) (e.0 u64)) u64
      (label (start)
        (set r.0 (const u64 1))
        (jump (loop b.0 e.0 r.0)))
      (label (body (b.1 u64) (e.1 u64) (r.1 u64))
        (set r.2 (add u64 r.1 b.1))
        (set one.0 (const u64 1))
        (set e.2 (add u64 e.1 one.0))
        (jump (loop b.1 e.2 r.2)))
      (label (loop (b.2 u64) (e.3 u64) (r.3 u64))
        (set z.0 (const u64 0))
        (set f.0 (cmp u64 gt e.3 z.0))
        (cond_jump f.0 (done r.3) (body b.2 e.3 r.3)))
      (label (done (r.4 u64))
        (ret r.4))) |}]
;;
