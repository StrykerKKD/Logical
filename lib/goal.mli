open Type

val equal : t -> t -> goal

val either : goal -> goal -> goal

val either_multi : goal list -> goal

val both : goal -> goal -> goal

val both_multi : goal list -> goal

val in_set : t -> (t, comparator_witness) Base.Set.t -> goal