open O
open Utils.Instr_types
include Types
module Interference = Interference

module Make (Config : Config) : sig
  module Allocation : Allocation with module Config := Config

  module type S = Make_S(Config)(Allocation).S

  module Greedy : S
  module Spill_all : S
end = struct
  open Config
  module Allocation = Make_allocation (Config)

  module type S = Make_S(Config)(Allocation).S

  module Greedy = Greedy.Make (Config)
  module Spill_all = Spill_all.Make (Config)
end
