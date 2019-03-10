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
  | ValueType.Set a_set, ValueType.Set b_set -> 
    if Base.Set.equal a_set b_set then Some state else None
  | a, b when a = b -> Some state
  | ValueType.Var variable, value -> Some ((variable, value) :: state)
  | value, ValueType.Var variable -> Some ((variable, value) :: state)
  | _, _ -> None

let assignment_to_string (variable, value) =
  "(" ^ variable ^ ValueType.to_string value ^ ")"

let to_string state =
  let assignments_content = Base.List.map state ~f:assignment_to_string in
  if List.length assignments_content <> 0 then
    let assignments = Base.List.concat [["(Assignments("]; assignments_content; ["))"]] in
    Base.String.concat ~sep:"" assignments
  else
    ""