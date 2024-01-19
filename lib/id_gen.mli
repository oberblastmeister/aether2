type 'k t

val create : (module Id.Key_type with type key = 'k) -> 'k t
val of_id : 'k Id.t -> 'k t
val to_id : 'k t -> 'k Id.t
val next : 'k t -> 'k Id.t
