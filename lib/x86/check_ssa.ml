open! O
open Ast

module Check = Compiler.Check_ssa.Make (struct
    module Instr = struct
      type t = VReg.t Instr.t [@@deriving sexp_of]

      let iter_uses i = Instr.iter_uses i |> F.Iter.map ~f:(fun v -> v.VReg.name)
      let iter_defs i = Instr.iter_defs i |> F.Iter.map ~f:(fun v -> v.VReg.name)
    end

    module Block = struct
      type t = VReg.t Block.t [@@deriving sexp_of]

      let iter_instrs_forward = Block.iter_instrs_forward
    end

    module Graph = struct
      type t = VReg.t Graph.t [@@deriving sexp_of]

      let start = Cfg.Graph.entry
      let find_block_exn graph label = Cfg.Graph.find_exn label graph
      let iteri_blocks = Cfg.Graph.iteri
      let to_double_graph graph = Graph.to_double_graph graph
    end

    module Function = struct
      type t = VReg.t Function.t [@@deriving sexp_of]

      let to_graph (fn : t) = fn.graph
      let params (fn : t) = List.map ~f:(fun param -> fst param |> VReg.name) fn.params
    end
  end)

let check program = Check.check_list program.Program.functions
