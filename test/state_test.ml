open Logical
open State

let test_create_with_empty_state () = 
  let expected = Some State.empty in
  let actual = create [] in
  Alcotest.(check int) "create should return an empty state"
    0 (compare expected actual)

let test_create_with_correct_assignment () = 
  let expected = Goal.equal (Value.var "a") (Value.int 0) State.empty |> Base.Sequence.to_list |> Base.List.hd_exn in
  let actual = create ["a", Value.int 0] in
  Alcotest.(check int) "create should return a state with the provided assignment"
    0 (compare expected actual)

let test_create_with_duplicated_assignments () = 
  let expected = None in
  let actual = create ["a", Value.int 0; "a", Value.int 1] in
  Alcotest.(check int) "create should return None for duplicated assignments"
    0 (compare expected actual)

let test_create_exn_with_empty_state () = 
  let expected = State.empty in
  let actual = create_exn [] in
  Alcotest.(check int) "create_exn should return an empty state"
    0 (compare expected actual)

let test_create_exn_with_correct_assignment () = 
  let expected = Goal.equal (Value.var "a") (Value.int 0) State.empty |> Base.Sequence.to_list |> Base.List.hd_exn |> Base.Option.value ~default:State.empty in
  let actual = create_exn ["a", Value.int 0] in
  Alcotest.(check int) "create_exn should return a state with the provided assignment"
    0 (compare expected actual)

let test_create_exn_with_duplicated_assignments () = 
  let expected = State.empty in
  let actual = try create_exn ["a", Value.int 0; "a", Value.int 1] with _ -> State.empty in
  Alcotest.(check int) "create_exn should throw exception for duplicated assignments"
    0 (compare expected actual)

let test_to_string_on_empty_state () = 
  let expected = "" in
  let actual = to_string empty in
  Alcotest.(check string) "to_string should return empty string for empty state" 
    expected actual

let test_to_string_on_variables () =
  let expected = "(Assignments((b(Int 2))(a(Int 1))))" in
  let actual = to_string (State.create_exn ["a",Value.int 1;"b",Value.int 2]) in
  Alcotest.(check string) "to_string should return the variables and it's values" 
    expected actual

let test_value_of_on_non_variable () =
  let expected = Value.int 1 in
  let actual = value_of empty (Value.int 1) in
  Alcotest.(check int) "value_of should return value directly in case of value input"
    0 (Type.compare expected actual)

let test_value_of_on_non_matching_variable () =
  let expected = Value.var "a" in
  let actual = value_of (State.create_exn ["b",Value.int 2]) (Value.var "a") in
  Alcotest.(check int) "value_of should return new variable in non matching case"
    0 (Type.compare expected actual)

let test_value_of_on_matching_variable () =
  let expected = Value.int 1 in
  let actual = value_of (State.create_exn ["a",Value.int 1]) (Value.var "a") in
  Alcotest.(check int) "value_of should return value in matching case"
    0 (Type.compare expected actual)

let test_unify_on_equal_variable_values () =
  let expected = Some (State.create_exn ["a",Value.int 1; "b",Value.int 1]) in
  let actual = unify (State.create_exn ["a",Value.int 1; "b",Value.int 1]) (Value.var "a") (Value.var "b") in
  Alcotest.(check int) "unify should return the a state which is equal to the input state"
    0 (compare expected actual)

let test_unify_on_new_left_variable () =
  let expected = Some (State.create_exn ["b",Value.int 1; "a",Value.int 1]) in
  let actual = unify (State.create_exn ["b",Value.int 1]) (Value.var "a") (Value.var "b") in
  Alcotest.(check int) "unify should return a new state with added left variable"
    0 (compare expected actual)

let test_unify_on_new_right_variable () =
  let expected = Some (State.create_exn ["a",Value.int 1; "b",Value.int 1]) in
  let actual = unify (State.create_exn ["a",Value.int 1]) (Value.var "a") (Value.var "b") in
  Alcotest.(check int) "unify should return a new state with added right variable"
    0 (compare expected actual)

let test_unify_on_non_equal_variable_values () =
  let expected = None in
  let actual = unify (State.create_exn ["a",Value.int 1; "b",Value.int 2]) (Value.var "a") (Value.var "b") in
  Alcotest.(check int) "unify should return None when variable value are not the same"
    0 (compare expected actual)

let test_unify_on_non_equal_values () =
  let expected = None in
  let actual = unify (State.create_exn ["a",Value.int 1; "b",Value.int 1]) (Value.int 1) (Value.int 2) in
  Alcotest.(check int) "unify should return None when both input is non variable"
    0 (compare expected actual)

let tests = [
  "test_create_with_empty_state", `Quick, test_create_with_empty_state;
  "test_create_with_correct_assignment", `Quick, test_create_with_correct_assignment;
  "test_create_with_duplicated_assignments", `Quick, test_create_with_duplicated_assignments;
  "test_create_exn_with_empty_state", `Quick, test_create_exn_with_empty_state;
  "test_create_exn_with_correct_assignment", `Quick, test_create_exn_with_correct_assignment;
  "test_create_exn_with_duplicated_assignments", `Quick, test_create_exn_with_duplicated_assignments;
  "test_to_string_on_empty_state", `Quick, test_to_string_on_empty_state;
  "test_to_string_on_variables", `Quick, test_to_string_on_variables;
  "test_value_of_on_non_variable", `Quick, test_value_of_on_non_variable;
  "test_value_of_on_non_matching_variable", `Quick, test_value_of_on_non_matching_variable;
  "test_value_of_on_matching_variable", `Quick, test_value_of_on_matching_variable;
  "test_unify_on_equal_variable_values", `Quick, test_unify_on_equal_variable_values;
  "test_unify_on_new_left_variable", `Quick, test_unify_on_new_left_variable;
  "test_unify_on_new_right_variable", `Quick, test_unify_on_new_right_variable;
  "test_unify_on_non_equal_variable_values", `Quick, test_unify_on_non_equal_variable_values;
  "test_unify_on_non_equal_values", `Quick, test_unify_on_non_equal_values;
]