open Logical
open State

let test_empty_state () = 
  Alcotest.(check bool) "empty should return empty list" true (empty = [])

let test_to_string_on_empty_state () = 
  Alcotest.(check string) "to_string should return empty string for empty state" 
  "" (to_string empty)

let test_to_string_on_non_empty_state () =
  Alcotest.(check string) "to_string should return specified value for nonempty state" 
  "(Assignments((a(Int 1))(b(Int 2))))" (to_string ["a",Value.int 1;"b",Value.int 2])

let test_value_of_on_non_variable () =
  let expected = Value.int 1 in
  let actual = value_of empty (Value.int 1) in
  Alcotest.(check bool) "value_of should return value directly in case of value input"
  true (expected = actual)

let test_value_of_on_non_matching_variable () =
  let expected = Value.var "a" in
  let actual = value_of ["b",Value.int 2] (Value.var "a") in
  Alcotest.(check bool) "value_of should return new variable in non matching case"
  true (expected = actual)

let test_value_of_on_matching_variable () =
  let expected = Value.int 1 in
  let actual = value_of ["a",Value.int 1] (Value.var "a") in
  Alcotest.(check bool) "value_of should return value in matching case"
  true (expected = actual)

let test_unify_on_equal_variable_values () =
  let expected = Some ["a",Value.int 1; "b",Value.int 1] in
  let actual = unify ["a",Value.int 1; "b",Value.int 1] 
    (Value.var "a") (Value.var "b")in
  Alcotest.(check bool) "unify should return the a state which is equal to the input state"
  true (expected = actual)

let test_unify_on_new_left_variable () =
  let expected = Some ["a",Value.int 1; "b",Value.int 1] in
  let actual = unify ["b",Value.int 1] 
    (Value.var "a") (Value.var "b")in
  Alcotest.(check bool) "unify should return a new state with added left variable"
  true (expected = actual)

let test_unify_on_new_right_variable () =
  let expected = Some ["b",Value.int 1; "a",Value.int 1] in
  let actual = unify ["a",Value.int 1] 
    (Value.var "a") (Value.var "b")in
  Alcotest.(check bool) "unify should return a new state with added right variable"
  true (expected = actual)

let test_unify_on_non_equal_variable_values () =
  let expected = None in
  let actual = unify ["a",Value.int 1; "b",Value.int 2] 
    (Value.var "a") (Value.var "b")in
  Alcotest.(check bool) "unify should return None when variable value are not the same"
  true (expected = actual)

let test_unify_on_non_equal_values () =
  let expected = None in
  let actual = unify ["a",Value.int 1; "b",Value.int 1] 
    (Value.int 1) (Value.int 2)in
  Alcotest.(check bool) "unify should return None when both input is non variable"
  true (expected = actual)

let test_unify_on_two_equal_set () =
  let test_set_a = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 1; Value.int 2; Value.int 3]) in
  let test_set_b = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 3; Value.int 2; Value.int 1]) in
  let expected = Some ["a",Value.int 1; "b",Value.int 1] in
  let actual = unify ["a",Value.int 1; "b",Value.int 1] 
    test_set_a test_set_b in
  Alcotest.(check bool) "unify should return state when both set is equal"
  true (expected = actual)

let test_unify_on_two_non_equal_set () =
  let test_set_a = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 1; Value.int 2; Value.int 3]) in
  let test_set_b = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 4; Value.int 5; Value.int 6]) in
  let expected = None in
  let actual = unify ["a",Value.int 1; "b",Value.int 1] 
    test_set_a test_set_b in
  Alcotest.(check bool) "unify should return None when both set is non equal"
  true (expected = actual)

let test_unify_on_set_and_value () =
  let test_set = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 1; Value.int 2; Value.int 3]) in
  let expected = None in
  let actual = unify ["a",Value.int 1; "b",Value.int 1] 
    test_set (Value.int 1) in
  Alcotest.(check bool) "unify should return none when input is a set and a value"
  true (expected = actual)

let test_unify_on_set_and_variable () =
  let test_set = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 1; Value.int 2; Value.int 3]) in
  let expected = "(Assignments((a(Set((Int 1)(Int 2)(Int 3))))))" in
  let actual = unify empty test_set (Value.var "a") 
    |> Base.Option.value ~default:[]
    |> State.to_string in
  Alcotest.(check string) "unify should return new state with set added to it"
  expected actual

let tests = [
  "test_empty_state", `Quick, test_empty_state;
  "test_to_string_on_empty_state", `Quick, test_to_string_on_empty_state;
  "test_to_string_on_non_empty_state", `Quick, test_to_string_on_non_empty_state;
  "test_value_of_on_non_variable", `Quick, test_value_of_on_non_variable;
  "test_value_of_on_non_matching_variable", `Quick, test_value_of_on_non_matching_variable;
  "test_value_of_on_matching_variable", `Quick, test_value_of_on_matching_variable;
  "test_unify_on_equal_variable_values", `Quick, test_unify_on_equal_variable_values;
  "test_unify_on_new_left_variable", `Quick, test_unify_on_new_left_variable;
  "test_unify_on_new_right_variable", `Quick, test_unify_on_new_right_variable;
  "test_unify_on_non_equal_variable_values", `Quick, test_unify_on_non_equal_variable_values;
  "test_unify_on_non_equal_values", `Quick, test_unify_on_non_equal_values;
  "test_unify_on_two_equal_set", `Quick, test_unify_on_two_equal_set;
  "test_unify_on_two_non_equal_set", `Quick, test_unify_on_two_non_equal_set;
  "test_unify_on_set_and_value", `Quick, test_unify_on_set_and_value;
  "test_unify_on_set_and_variable", `Quick, test_unify_on_set_and_variable;
]