open! O
module Lir = Lir_imports

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
      F.Fold.of_fn Function.graph
      @> F.Fold.of_fn Cfg_graph.blocks
      @> F.Core.Map.fold
      @> Block.instrs_forward_fold
      @> Some_instr.uses_fold)
  in
  let uses = F.Fold.reduce p F.Reduce.to_list_rev fn in
  print_s [%sexp (uses : Lir.Value.t list)];
  [%expect
    {|
    (((name (Name f)) (ty U1)) ((name (Name z)) (ty U64))
     ((name (Name e)) (ty U64)) ((name (Name r)) (ty U64))
     ((name (Name one)) (ty U64)) ((name (Name e)) (ty U64))
     ((name (Name b)) (ty U64)) ((name (Name r)) (ty U64))) |}]
;;

let%expect_test "liveness" =
  let fn = List.hd_exn (Lazy.force loop_lir).functions in
  let res = Vir.Liveness.run fn.graph in
  print_s [%sexp (res : Lir.Value.Set.t Lir.Label.Map.t)];
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
  let res = Vir.DataflowDominators.run fn.graph in
  print_s [%sexp (res : Lir.Label.Set.t Lir.Label.Map.t)];
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
  let dominators = Vir.DataflowDominators.run fn.graph in
  let idoms = dominators |> Dataflow.Dominators.compute_idoms_from_facts fn.graph.entry in
  let idom_tree =
    dominators |> Dataflow.Dominators.compute_idom_tree_from_facts fn.graph.entry
  in
  print_s [%sexp "idoms", (idoms : Lir.Label.t Lir.Label.Map.t)];
  print_s [%sexp "idom_tree", (idom_tree : Lir.Label.Set.t Lir.Label.Map.t)];
  [%expect
    {|
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
  let idoms = Lir.Graph.get_idoms fn.graph in
  print_s [%sexp (idoms : Lir.Label.t Lir.Label.Hashtbl.t)];
  ();
  [%expect
    {|
    ((((name (Name body))) ((name (Name loop))))
     (((name (Name done))) ((name (Name loop))))
     (((name (Name loop))) ((name (Name start))))
     (((name (Name start))) ((name (Name start))))) |}]
;;

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
    (define (pow (b.0 u64) (e.1 u64)) u64
      (label (start)
        (set r.2 (const u64 1))
        (jump (loop b.0 e.1 r.2)))
      (label (body (b.3 u64) (e.4 u64) (r.5 u64))
        (set r.6 (add u64 r.5 b.3))
        (set one.7 (const u64 1))
        (set e.8 (add u64 e.4 one.7))
        (jump (loop b.3 e.8 r.6)))
      (label (loop (b.9 u64) (e.10 u64) (r.11 u64))
        (set z.12 (const u64 0))
        (set f.13 (cmp u64 gt e.10 z.12))
        (cond_jump f.13 (done r.11) (body b.9 e.10 r.11)))
      (label (done (r.14 u64))
        (ret r.14))) |}]
;;

let%expect_test "ssa" =
  let program = Lazy.force loop_lir |> Lir.Ssa.convert_ssa in
  print_endline @@ Lir.pretty program;
  [%expect
    {|
    (define (pow (b.0 u64) (e.1 u64)) u64
      (label (start)
        (set r.2 (const u64 1))
        (jump (loop e.1 r.2)))
      (label (body)
        (set r.6 (add u64 r.11 b.0))
        (set one.7 (const u64 1))
        (set e.8 (add u64 e.10 one.7))
        (jump (loop e.8 r.6)))
      (label (loop (e.10 u64) (r.11 u64))
        (set z.12 (const u64 0))
        (set f.13 (cmp u64 gt e.10 z.12))
        (cond_jump f.13 (done) (body)))
      (label (done)
        (ret r.11))) |}]
;;

(*
   let%expect_test "lower" =
  let program = Lazy.force loop_lir |> Lir.Ssa.convert_ssa |> Lir.Lower.lower_program in
  print_endline @@ Lir.Lower.pretty program;
  [%expect
    {|
    (define (pow (b.0 u64) (e.1 u64)) u64
      (label (start)
        (jump (loop e.1 (const u64 1))))
      (label (body)
        (jump (loop (add u64 e.10 (const u64 1)) (add u64 r.11 b.0))))
      (label (loop (e.10 u64) (r.11 u64))
        (cond_jump (cmp u64 gt e.10 (const u64 0)) (done) (body)))
      (label (done)
        (ret r.11))) |}]
;; *)
