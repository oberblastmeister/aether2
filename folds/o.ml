let ( @> ) = Fold.( @> )
let ( & ) = Map.( & )

module Folds = struct
  module Fold = Fold
  module Iter = Iter
  module Reduce = Reduce
  module Map = Map
end
