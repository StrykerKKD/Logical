open Logical
open Goal

let test_equal_on_value_variable_and_empty_state () =
  let expected = [Some (State.create_exn ["a",Value.int 1])] in
  let actual = equal (Value.var "a") (Value.int 1) State.empty |> Base.Sequence.to_list in
  Alcotest.(check int) "equal should return with state which contains the value variable" 
    0 (compare expected actual)

let test_equal_on_value_variable_and_matching_state () =
  let expected = [Some (State.create_exn ["b",Value.int 1;"a",Value.int 1])] in
  let actual = equal (Value.var "a") (Value.var "b") (State.create_exn ["b",Value.int 1]) |> Base.Sequence.to_list in
  Alcotest.(check int) "equal should return with state which contains the value variable" 
    0 (compare expected actual)

let test_either_on_variables_and_empty_state () =
  let expected = [Some (State.create_exn ["a",Value.int 1]); Some (State.create_exn ["b",Value.int 2])] in
  let actual = either 
    (equal (Value.var "a") (Value.int 1)) (equal (Value.var "b") (Value.int 2)) State.empty
    |> Base.Sequence.to_list in
  Alcotest.(check int) "either should return goal results in different states" 
    0 (compare expected actual)

let test_either_multi_on_variables_and_empty_state () =
  let expected = [Some (State.create_exn ["a",Value.int 1]); Some (State.create_exn ["b",Value.int 2])] in
  let actual = either_multi
    [equal (Value.var "a") (Value.int 1); equal (Value.var "b") (Value.int 2)] State.empty
    |> Base.Sequence.to_list in
  Alcotest.(check int) "either_multi should return goal results in different states" 
    0 (compare expected actual)

let test_both_on_variables_and_empty_state () =
  let expected = [Some (State.create_exn ["a",Value.int 1; "b",Value.int 2])] in
  let actual = both 
    (equal (Value.var "a") (Value.int 1)) (equal (Value.var "b") (Value.int 2)) State.empty
    |> Base.Sequence.to_list in
  Alcotest.(check int) "both should return goal results in one state" 
    0 (compare expected actual)

let test_both_multi_on_variables_and_empty_state () =
  let expected = [Some (State.create_exn ["a",Value.int 1; "b",Value.int 2])] in
  let actual = both_multi 
    [equal (Value.var "a") (Value.int 1); equal (Value.var "b") (Value.int 2)] State.empty
    |> Base.Sequence.to_list in
  Alcotest.(check int) "both should return goal results in one state" 
    0 (compare expected actual)

let test_in_set_on_variables_and_empty_state () =
  let test_set = Base.Set.of_list (module Type) [Value.int 1; Value.int 2] in
  let expected = [Some (State.create_exn ["a",Value.int 1]); Some (State.create_exn ["a",Value.int 2])] in
  let actual = in_set (Value.var "a") test_set State.empty |> Base.Sequence.to_list in
  Alcotest.(check int) "in_set should return goal result like either, but with only one variable" 
    0 (compare expected actual)

let tests = [
  "test_equal_on_value_variable_and_empty_state", `Quick, test_equal_on_value_variable_and_empty_state;
  "test_equal_on_value_variable_and_matching_state", `Quick, test_equal_on_value_variable_and_matching_state;
  "test_either_on_variables_and_empty_state", `Quick, test_either_on_variables_and_empty_state;
  "test_either_multi_on_variables_and_empty_state", `Quick, test_either_multi_on_variables_and_empty_state;
  "test_both_on_variables_and_empty_state", `Quick, test_both_on_variables_and_empty_state;
  "test_both_multi_on_variables_and_empty_state", `Quick, test_both_multi_on_variables_and_empty_state;
  "test_in_set_on_variables_and_empty_state", `Quick, test_in_set_on_variables_and_empty_state;
]