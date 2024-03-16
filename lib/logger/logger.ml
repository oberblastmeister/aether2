module Ppx_log_syntax = Ppx_log_syntax

let with_log flag f =
  let old = !Log.would_log in
  Log.would_log := flag;
  let res = f () in
  Log.would_log := old;
  res
;;
