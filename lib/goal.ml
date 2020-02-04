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

let both_multi goals = (fun state ->
  match Base.List.reduce goals ~f:both with
  | Some combined_goal -> combined_goal state
  | None -> Base.Sequence.empty
)

let in_set value set = (fun state -> 
  let assingmentGoals = Base.Set.to_list set |> Base.List.map ~f:(fun element -> equal value element) in
  either_multi assingmentGoals state
)