include Core
include Folds.O
module F = Folds
module FC = F.Core

let ( |- ) f x =
  f x;
  x
;;

let todo () = failwith "TODO"
