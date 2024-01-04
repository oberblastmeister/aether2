open Instr_types

module Graph = struct
  type 'b t = { entry : Label.t; blocks : 'b Label.Map.t; exit : Label.t }
  [@@deriving sexp_of, fields]

  module Fields = struct
    let entry = entry
    let blocks = blocks
    let exit = exit
    module Fields = Fields
  end
end
