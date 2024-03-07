type 'k t [@@deriving sexp_of]

val create : ?next_id:'k Id.t ->  'k Id.key_proxy -> 'k t
val id_of_string : 'k t -> string -> 'k Id.t
val name_of_string : 'k t -> string -> 'k Name.t
val get_next_id : 'k t -> 'k Id.t
