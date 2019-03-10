(*type variable = string

type assignment = variable * Value.Type.t

type state = assignment list

type goal = state -> state Base.Sequence.t*)