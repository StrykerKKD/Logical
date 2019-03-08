open Logical

let my_set = Base.Set.of_list (module ValueComparator) [ValueType.Int 1; ValueType.Int 2; ValueType.Int 3]
let example_goal = in_set_goal (ValueType.Var "a") (ValueType.Set my_set)
let result = pursue_goal example_goal empty_state
let result_list = Base.Sequence.to_list_rev result
let () = Base.List.iter result_list ~f:(fun element ->
  match element with
  | Some value -> state_to_string value |> print_endline
  | None -> ()
)