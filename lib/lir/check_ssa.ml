open! O
open Ast

module Check = Compiler.Check_ssa.Make (struct
    module Instr = struct
      type t = Value.t Some_instr.t [@@deriving sexp_of]

      let iter_uses i = Some_instr.iter_uses i |> F.Iter.map ~f:(fun v -> v.Value.name)
      let iter_defs i = Some_instr.iter_defs i |> F.Iter.map ~f:(fun v -> v.Value.name)
    end

    module Block = struct
      type t = Value.t Block.t [@@deriving sexp_of]

      let iter_instrs_forward = Block.iter_instrs_forward
    end

    module Graph = struct
      type t = Value.t Graph.t [@@deriving sexp_of]

      let start = Cfg.Graph.entry
      let find_block_exn graph label = Cfg.Graph.find_exn label graph
      let iteri_blocks = Cfg.Graph.iteri
      let to_double_graph graph = Graph.to_double_graph graph
    end

    module Function = struct
      type t = Value.t Function.t [@@deriving sexp_of]

      let to_graph (fn : t) = fn.graph
      let params (fn : t) = List.map ~f:(fun v -> v.name) fn.ty.params
    end
  end)

let check modul =
  Check.check_list (F.Fold.to_list (Module.iter_decls @> Decl.iter_func) modul)
;;
