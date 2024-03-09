type t

let would_log = ref false

let with_log flag f =
  let old = !would_log in
  would_log := flag;
  let res = f () in
  would_log := old;
  res
;;
