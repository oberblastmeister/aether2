open O
open Types

(* a color is an instruction index modulo pure operations *)
module Color = struct
  type t = int [@@deriving sexp_of]

  let is_before c1 c2 = c1 + 1 = c2
end

module Color_of_index = struct
  type t = (Color.t, Perms.Read.t) Vec.t [@@deriving sexp_of]

  let get = Vec.get
end

let color (graph : _ Cfg.Graph.t) =
  let cur_color = ref 0 in
  let colors = Vec.create () in
  FC.Map.fold graph.blocks (fun block ->
    incr cur_color;
    Block.instrs_forward_fold block (fun instr ->
      Vec.push colors !cur_color;
      if Some_instr.has_side_effect instr then incr cur_color));
  colors |> Vec.freeze
;;
