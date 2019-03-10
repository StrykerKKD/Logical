open Value

val equal : Type.t -> Type.t -> goal

val either : goal -> goal -> goal

val either_multi : goal list -> goal

val both : goal -> goal -> goal

val in_set : Type.t -> Type.t -> goal