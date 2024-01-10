let ( @> ) = Fold.( @> )
let ( & ) = Map.( & )

module Folds = struct
  module Fold = Fold
  module Iter = Iter
  module Reduce = Reduce
  module Map = Map

  module Core = struct
    include Core_instances
  end
end
