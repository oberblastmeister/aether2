module Core = struct
  include Core
end

include Core
include Folds.O
module F = Folds
module FC = F.Core
module Data = Aether_data
module Vec = Data.Vec
module List1 = Data.List1
module Entity = Aether_entity

let ( |- ) x f =
  f x;
  x
;;

let todo () = failwith "TODO"
