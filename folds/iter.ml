include IterLabels

let enumerate it k =
  let ix = ref 0 in
  it (fun x ->
    k (!ix, x);
    incr ix);
  ()
;;

let of_lab (it : f:('a -> unit) -> unit) k = it ~f:k
