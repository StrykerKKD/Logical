open Value

let empty = []

let rec value_of state value =
  match value with
  | ValueType.Var variable -> 
    (match List.assoc_opt variable state with
    | Some value -> value_of state value
    | None -> ValueType.Var variable)
  | _ -> value
  
let unify state value_a value_b =
  let new_value_a = value_of state value_a in
  let new_value_b = value_of state value_b in
  match new_value_a, new_value_b with
  | a, b when a = b -> Some state
  | ValueType.Var variable, value -> Some ((variable, value) :: state)
  | value, ValueType.Var variable -> Some ((variable, value) :: state)
  | _, _ -> None

let assignment_to_string (variable, value) =
  variable ^ ": " ^ ValueType.to_string value

let to_string state =
  let assignments = Base.List.map state ~f:assignment_to_string in
  if List.length assignments <> 0 then
    Base.String.concat ~sep:"\n" ("Assignments:" :: assignments)
  else
    ""