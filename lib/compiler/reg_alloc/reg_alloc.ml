include Types
module Interference = Interference

(* the optimal algorithm *)
module Make = Greedy.Make
module Greedy = Greedy
module Spill_all = Spill_all
