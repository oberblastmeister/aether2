include IterLabels

let enumerate it k =
  let ix = ref 0 in
  it (fun x ->
    k (!ix, x);
    incr ix);
  ()
;;
