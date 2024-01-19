open O
open Data_graph_types

val t_of_double : 'a double -> 'a t
val of_map_generic : iter:('a -> 'b Iter.t) -> ('b, 'a, 'c) Map.t -> 'b t
val t_of_map_list : ('a, 'a list, 'b) Map.t -> 'a t
val t_of_map_set : ('a, ('a, 'b) Set.t, 'c) Map.t -> 'a t

val get_pred_map
  :  ('k, 'h) Constructors.map
  -> 'k t
  -> ('k -> 'k list -> 'h) Higher_kinded.t

val double_of_t : 'k Constructors.some_map -> 'k t -> 'k double
