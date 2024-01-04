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
   |> Lir.parse |> Or_error.ok_exn)

let%expect_test "uses" =
  let fn = List.hd_exn @@ Lazy.force loop_lir in
  let p =
    Lir.(
      Function.body @> Graph.blocks @> A.Map.each
      @> Block.instrs_forward_accessor)
  in
  let uses =
    A.fold p ~f:(fun z (Lir.Instr.Some.T i) -> Lir.Instr.uses i @ z) ~init:[] fn
  in
  print_s [%sexp (uses : Lir.Value.t list)];
  [%expect
    {|
    (((name (Name z)) (ty U64)) ((name (Name e)) (ty U64))
     ((name (Name f)) (ty U64)) ((name (Name r)) (ty U64))
     ((name (Name b)) (ty U64)) ((name (Name r)) (ty U64))
     ((name (Name one)) (ty U64)) ((name (Name e)) (ty U64))) |}]

let%expect_test "liveness" =
  let fn = List.hd_exn (Lazy.force loop_lir) in
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

let%expect_test "dominators" =
  let fn = List.hd_exn (Lazy.force loop_lir) in
  let res = Lir.Dominators.run fn.body in
  print_s [%sexp (res : Lir.Dominators.BlockTransfer.domain Lir.Label.Map.t)];
  ();
  [%expect
    {|
    ((((name (Name body)))
      (((name (Name body))) ((name (Name loop))) ((name (Name start)))))
     (((name (Name done)))
      (((name (Name done))) ((name (Name loop))) ((name (Name start)))))
     (((name (Name loop))) (((name (Name loop))) ((name (Name start)))))
     (((name (Name start))) (((name (Name start)))))) |}]
