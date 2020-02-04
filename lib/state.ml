let empty = Base.Map.empty (module Base.String)

let create assignments = 
  match Base.Map.of_alist (module Base.String) assignments with
  | `Ok result -> Some result
  | `Duplicate_key _ -> None

let create_exn assignments = 
  Base.Map.of_alist_exn (module Base.String) assignments

let rec value_of state value =
  match value with
  | Value.Type.Var variable_name -> 
    (match Base.Map.find state variable_name with
    | Some value -> value_of state value
    | None -> Value.Type.Var variable_name)
  | _ -> value

let add_to_state state variable_name value =
  match Base.Map.add state ~key:variable_name ~data:value with
  | `Ok result -> Some result
  | `Duplicate -> None

let unify state value_a value_b =
  let new_value_a = value_of state value_a in
  let new_value_b = value_of state value_b in
  match new_value_a, new_value_b with
  | a, b when (Value.Type.compare a b) = 0 -> Some state
  | Value.Type.Var variable_name, value -> add_to_state state variable_name value
  | value, Value.Type.Var variable_name -> add_to_state state variable_name value
  | _, _ -> None

let assignment_to_string ~key ~data acc =
  Base.List.cons ("(" ^ key ^ Value.to_string data ^ ")") acc

let to_string state =
  let assignments_content = Base.Map.fold state ~init:[] ~f:assignment_to_string in
  if Base.List.length assignments_content <> 0 then
    let assignments = Base.List.concat [["(Assignments("]; assignments_content; ["))"]] in
    Base.String.concat ~sep:"" assignments
  else
    ""