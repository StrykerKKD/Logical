open Value

let equal value_a value_b = (fun state -> 
  let state = State.unify state value_a value_b in
  Base.Sequence.singleton state
)

let either first_goal second_goal = (fun state -> 
  let first_stream = first_goal state in
  let second_stream = second_goal state in
  Base.Sequence.round_robin [first_stream; second_stream]
)

let either_multi goals = (fun state -> 
  let streams = Base.List.map goals ~f:(fun goal -> goal state) in
  Base.Sequence.round_robin streams
)

let both first_goal second_goal = (fun state ->
  let first_states = first_goal state in
  let sum_states = Base.Sequence.filter_map first_states ~f:(fun first_state -> 
    match first_state with
    | Some state -> Some (second_goal state)
    | None -> None
  ) in
  Base.Sequence.interleave sum_states
)

let pursue_desugared_set set variable state =
  let value_list = Base.Set.to_list set in
  let both_goals = Base.List.map value_list ~f:(fun value -> equal (ValueType.Var variable) value) in
  let either_goal = either_multi both_goals in 
  either_goal state

let in_set value_a value_b = (fun state -> 
  match value_a, value_b with
  | ValueType.Var variable, ValueType.Set set -> pursue_desugared_set set variable state
  | ValueType.Set set, ValueType.Var variable -> pursue_desugared_set set variable state
  | _ -> Base.Sequence.singleton None
)