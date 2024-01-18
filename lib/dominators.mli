open! O
open Instr_types

type 'n idoms = ('n, 'n) Hashtbl.t

val get_idoms : Label.t -> Label.t Data_graph.double -> Label.t idoms

val frontier_of_idoms
  :  Label.t idoms
  -> Label.t Data_graph.double
  -> (Label.t, Label.t Hash_set.t) Hashtbl.t
