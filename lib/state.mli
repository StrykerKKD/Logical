open Type

val empty : state

val create : (variable_name * t) list -> state option

val create_exn : (variable_name * t) list -> state

val value_of : state -> t -> t

val unify : state -> t -> t -> state option

val to_string : state -> string