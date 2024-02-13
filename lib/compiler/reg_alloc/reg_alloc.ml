open O
open Utils.Instr_types
include Types
module Interference = Interference

module Make (Config : Config) : sig
  module Allocation : Allocation(Config).S

  module type Algorithm = Algorithm(Config)(Allocation).S

  module Greedy : Algorithm
  module Spill_all : Algorithm
end = struct
  open Config
  module Allocation = Make_allocation (Config)

  module type Algorithm = Algorithm(Config)(Allocation).S

  module Greedy = Greedy.Make (Config)
  module Spill_all = Spill_all.Make (Config)
end
