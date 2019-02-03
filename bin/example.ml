type variable = string

type value =
  | Integer of int
  | Float of float
  | String of string
  | Boolean of bool
  | Variable of variable

type 'a state = {
  variables: variable list;
  values: (variable * value) list;
}

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

let rec value_of_state state variable =
  let values = state.values in
  match List.assoc_opt variable values with
  | Some (Variable value) -> value_of_state state value
  | Some value -> value
  | None -> Variable variable

let unify_state state variable_a variable_b =
  let a = value_of_state state variable_a in
  let b = value_of_state state variable_b in
  match a, b with
  | a, b when a = b -> Some state
  | Variable a, _ -> Some (assing_values state [(a, b)])
  | _, Variable b -> Some (assing_values state [(b, a)])
  | _, _ -> None
