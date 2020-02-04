open Value

val empty : state

val create : (variable_name * Type.t) list -> state option

val create_exn : (variable_name * Type.t) list -> state

val value_of : state -> Type.t -> Type.t

val unify : state -> Type.t -> Type.t -> state option

val to_string : state -> string