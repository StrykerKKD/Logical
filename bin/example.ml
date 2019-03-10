open Logical

let my_set = Base.Set.of_list (module Value.Comparator) [Value.int 1; Value.int 2; Value.int 3]
let example_goal = Goal.in_set (Value.var "a") (Value.set my_set)
let result = example_goal State.empty
let result_list = Base.Sequence.to_list_rev result
let () = Base.List.iter result_list ~f:(fun element ->
  match element with
  | Some value -> State.to_string value |> print_endline
  | None -> ()
)