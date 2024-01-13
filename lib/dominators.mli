open O

type 'n idoms = ('n, 'n) Hashtbl.t

val get_idoms : 'n -> 'n Data_graph.double -> 'n idoms
val frontier_of_idoms : 'n idoms -> 'n Data_graph.double -> ('n, 'n Hash_set.t) Hashtbl.t
