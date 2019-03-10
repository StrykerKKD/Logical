open Value

val empty : 'a list

val value_of : state -> Type.t -> Type.t

val unify : state -> Type.t -> Type.t -> state option

val to_string : state -> string