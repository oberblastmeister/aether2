open Instr_types

module Graph = struct
  type 'b t = { entry : Label.t; body : 'b Label.Map.t; exit : Label.t }
  [@@deriving accessors, sexp_of]
end

module MakeGraph (Block : sig
  type t [@@deriving sexp_of]
end) =
struct
  type t = Block.t Graph.t [@@deriving sexp_of]
end
