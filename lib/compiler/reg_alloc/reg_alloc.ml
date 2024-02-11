include Types
module Interference = Interference
module Spill_all = Spill_all

module Make (Config : Config) = struct
  module Arg = Make_arg (Config)

  module type S = S with module Arg := Arg

  module Allocation = Arg.Allocation
  module Greedy = Greedy.Make (Arg)
  module Spill_all = Spill_all.Make (Arg)
end
