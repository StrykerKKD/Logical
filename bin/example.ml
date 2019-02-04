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

type goal = state -> state Stream.t

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
  Stream.of_list [state]
)