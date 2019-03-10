open Logical

let print_state_stream title state_stream =
  print_endline title;
  let state_list = Base.Sequence.to_list state_stream in
  Base.List.iter state_list ~f:(fun element ->
    match element with
    | Some value -> State.to_string value |> print_endline
    | None -> ()
  );
  print_newline ()

let equal_result = Goal.equal (Value.var "a") (Value.int 1) State.empty
let () = print_state_stream "equal_result" equal_result

let either_result = Goal.either
  (Goal.equal (Value.var "a") (Value.int 1))
  (Goal.equal (Value.var "b") (Value.int 2))
  State.empty
let () = print_state_stream "either_result" either_result

let either_result_multi = Goal.either_multi
  [Goal.equal (Value.var "a") (Value.int 1); Goal.equal (Value.var "b") (Value.int 2);
  Goal.equal (Value.var "b") (Value.int 3);]
  State.empty
let () = print_state_stream "either_result_multi" either_result_multi

let both_result = Goal.both
  (Goal.equal (Value.var "a") (Value.int 1))
  (Goal.equal (Value.var "b") (Value.int 2))
  State.empty
let () = print_state_stream "both_result" both_result

let my_set = Base.Set.of_list (module Value.Comparator) [Value.int 1; Value.int 2; Value.int 3]
let in_set_result = Goal.in_set (Value.var "a") (Value.set my_set) State.empty
let () = print_state_stream "in_set_result" in_set_result