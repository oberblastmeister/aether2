open! O
module Lir = Ast

module Instantiate (V : sig
    type t [@@deriving sexp_of]
  end) =
struct
  module Instr = struct
    type t = V.t Lir.Instr.t [@@deriving sexp_of]
  end

  module Block_args = struct
    type t = Lir.Block_args.t [@@deriving sexp_of]
  end

  module Block_call = struct
    type t = V.t Lir.Block_call.t [@@deriving sexp_of]
  end

  module Control_instr = struct
    type t = V.t Lir.Control_instr.t [@@deriving sexp_of]
  end

  module Some_instr = struct
    type t = V.t Lir.Some_instr.t [@@deriving sexp_of]
  end

  module Generic_instr = struct
    type 'c t = (V.t, 'c) Lir.Generic_instr.t
  end

  module Block = struct
    type t = V.t Lir.Block.t [@@deriving sexp_of]
  end

  module Graph = struct
    type t = V.t Lir.Graph.t [@@deriving sexp_of]
  end

  module Mut_function = struct
    type t = V.t Lir.Mut_function.t [@@deriving sexp_of]
  end

  module Function = struct
    type t = V.t Lir.Function.t [@@deriving sexp_of]
  end

  module Module = struct
    type t = V.t Lir.Module.t [@@deriving sexp_of]
  end
end
