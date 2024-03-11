let () = print_endline "Hello, World!"

(* module Utils = struct
  module type Ops = sig
    val testing : 'a -> 'a
  end

  module Testing : Ops = struct
    let[@inline always] testing x = x
  end
  
  module Ops : Ops = Testing
end *)
