let ( @> ) = Fold.( @> )
let ( & ) = Map.( & )

module Getter = struct
  module Fold = Fold
  module Reduce = Reduce
  module Traverse = Traverse
  module Scan = Scan
  module Map = Map

  module Core = struct
    include Core_instances
  end
end
