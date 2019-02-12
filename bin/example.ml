type variable = string

type value =
  | Integer of int
  | Float of float
  | String of string
  | Boolean of bool
  | Variable of variable

type state = {
  variables: variable list;
  values: (variable * value) list;
}

type goal = state -> state Base.Sequence.t

let empty_state = {
  variables = [];
  values = [];
}

let create_variables state new_variables = {
  state with variables = List.append state.variables new_variables
}

let assing_values state new_values = {
  state with values = List.append state.values new_values
}

let create_state variables values = {
  variables;
  values;
}

let rec value_of_state state value =
  match value with
  | Variable variable -> 
    let values = state.values in
    (match List.assoc_opt variable values with
    | Some value -> value_of_state state value
    | None -> Variable variable)
  | _ -> value
  

let unify_state state value_a value_b =
  let new_value_a = value_of_state state value_a in
  let new_value_b = value_of_state state value_b in
  match new_value_a, new_value_b with
  | a, b when a = b -> Some state
  | Variable a, _ -> Some (assing_values state [a, new_value_b])
  | _, Variable b -> Some (assing_values state [b, new_value_a])
  | _, _ -> None

let pursue_goal goal state =
  goal state

let equal_goal value_a value_b = (fun state -> 
  let state = unify_state state value_a value_b in
  Base.Sequence.singleton state
)

let with_variables_goal variables goal_maker = (fun state ->
  let new_state = create_variables state variables in
  let goal = goal_maker () in
  pursue_goal goal new_state
)

let either_goal first_goal second_goal = (fun state -> 
  let first_stream = pursue_goal first_goal state in
  let second_stream = pursue_goal second_goal state in
  Base.Sequence.round_robin [first_stream; second_stream]
)

let either_goal_general goals = (fun state -> 
  let streams = Base.List.map goals ~f:(fun goal -> pursue_goal goal state) in
  Base.Sequence.round_robin streams
)

let both_goal first_goal second_goal = (fun state ->
  let first_states = pursue_goal first_goal state in
  let sum_states = Base.Sequence.filter_map first_states ~f:(fun first_state -> 
    match first_state with
    | Some state -> Some (pursue_goal second_goal state)
    | None -> None
  ) in
  Base.Sequence.interleave sum_states
)