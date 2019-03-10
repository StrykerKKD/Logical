let empty = []

let rec value_of state value =
  match value with
  | Value.Type.Var variable -> 
    (match List.assoc_opt variable state with
    | Some value -> value_of state value
    | None -> Value.Type.Var variable)
  | Value.Type.Set set -> 
    Base.Set.map (module Value.Comparator) set ~f:(value_of state) |> Value.set
  | _ -> value
  
let unify state value_a value_b =
  let new_value_a = value_of state value_a in
  let new_value_b = value_of state value_b in
  match new_value_a, new_value_b with
  | a, b when (Value.Type.compare a b) = 0 -> Some state
  | Value.Type.Var variable, value -> Some ((variable, value) :: state)
  | value, Value.Type.Var variable -> Some ((variable, value) :: state)
  | _, _ -> None

let assignment_to_string (variable, value) =
  "(" ^ variable ^ Value.Type.to_string value ^ ")"

let to_string state =
  let assignments_content = Base.List.map state ~f:assignment_to_string in
  if List.length assignments_content <> 0 then
    let assignments = Base.List.concat [["(Assignments("]; assignments_content; ["))"]] in
    Base.String.concat ~sep:"" assignments
  else
    ""