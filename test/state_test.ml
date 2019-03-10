open Logical
open State

let test_empty_state () = 
  Alcotest.(check int) "empty should return empty list" 
    0 (compare empty [])

let test_to_string_on_empty_state () = 
  let expected = "" in
  let actual = to_string empty in
  Alcotest.(check string) "to_string should return empty string for empty state" 
    expected actual

let test_to_string_on_variables () =
  let expected = "(Assignments((a(Int 1))(b(Int 2))))" in
  let actual = to_string ["a",Value.int 1;"b",Value.int 2] in
  Alcotest.(check string) "to_string should return the variables and it's values" 
    expected actual

let test_to_string_on_set () =
  let test_set = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 1; Value.int 2; Value.int 3]) in
  let expected = "(Assignments((a(Set((Int 1)(Int 2)(Int 3))))))" in
  let actual = to_string ["a", test_set] in
  Alcotest.(check string) "to_string should return the set and it's values"
    expected actual

let test_value_of_on_non_variable () =
  let expected = Value.int 1 in
  let actual = value_of empty (Value.int 1) in
  Alcotest.(check int) "value_of should return value directly in case of value input"
    0 (Value.Type.compare expected actual)

let test_value_of_on_non_matching_variable () =
  let expected = Value.var "a" in
  let actual = value_of ["b",Value.int 2] (Value.var "a") in
  Alcotest.(check int) "value_of should return new variable in non matching case"
    0 (Value.Type.compare expected actual)

let test_value_of_on_matching_variable () =
  let expected = Value.int 1 in
  let actual = value_of ["a",Value.int 1] (Value.var "a") in
  Alcotest.(check int) "value_of should return value in matching case"
    0 (Value.Type.compare expected actual)

let test_value_of_on_set_with_matching_variable () =
  let test_set = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.var "a"; Value.int 2]) in
  let expected = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 1; Value.int 2]) in
  let actual = value_of ["a",Value.int 1] test_set in
  Alcotest.(check int) "value_of should return new set with the substituted value"
    0 (Value.Type.compare expected actual)

let test_unify_on_equal_variable_values () =
  let expected = Some ["a",Value.int 1; "b",Value.int 1] in
  let actual = unify ["a",Value.int 1; "b",Value.int 1] (Value.var "a") (Value.var "b") in
  Alcotest.(check int) "unify should return the a state which is equal to the input state"
    0 (compare expected actual)

let test_unify_on_new_left_variable () =
  let expected = Some ["a",Value.int 1; "b",Value.int 1] in
  let actual = unify ["b",Value.int 1] (Value.var "a") (Value.var "b") in
  Alcotest.(check int) "unify should return a new state with added left variable"
    0 (compare expected actual)

let test_unify_on_new_right_variable () =
  let expected = Some ["b",Value.int 1; "a",Value.int 1] in
  let actual = unify ["a",Value.int 1] (Value.var "a") (Value.var "b") in
  Alcotest.(check int) "unify should return a new state with added right variable"
    0 (compare expected actual)

let test_unify_on_non_equal_variable_values () =
  let expected = None in
  let actual = unify ["a",Value.int 1; "b",Value.int 2] (Value.var "a") (Value.var "b") in
  Alcotest.(check int) "unify should return None when variable value are not the same"
    0 (compare expected actual)

let test_unify_on_non_equal_values () =
  let expected = None in
  let actual = unify ["a",Value.int 1; "b",Value.int 1] (Value.int 1) (Value.int 2) in
  Alcotest.(check int) "unify should return None when both input is non variable"
    0 (compare expected actual)

let test_unify_on_two_equal_set () =
  let test_set_a = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 1; Value.int 2; Value.int 3]) in
  let test_set_b = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 3; Value.int 2; Value.int 1]) in
  let expected = Some ["a",Value.int 1; "b",Value.int 1] in
  let actual = unify ["a",Value.int 1; "b",Value.int 1] test_set_a test_set_b in
  Alcotest.(check int) "unify should return state when both set is equal"
    0 (compare expected actual)

let test_unify_on_two_non_equal_set () =
  let test_set_a = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 1; Value.int 2; Value.int 3]) in
  let test_set_b = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 4; Value.int 5; Value.int 6]) in
  let expected = None in
  let actual = unify ["a",Value.int 1; "b",Value.int 1] test_set_a test_set_b in
  Alcotest.(check int) "unify should return None when both set is non equal"
    0 (compare expected actual)

let test_unify_on_set_and_value () =
  let test_set = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 1; Value.int 2; Value.int 3]) in
  let expected = None in
  let actual = unify ["a",Value.int 1; "b",Value.int 1] test_set (Value.int 1) in
  Alcotest.(check int) "unify should return none when input is a set and a value"
    0 (compare expected actual)

let test_unify_on_set_and_variable () =
  let test_set = Value.set (Base.Set.of_list (module Value.Comparator) 
    [Value.int 1; Value.int 2; Value.int 3]) in
  let expected = Some ["a", test_set] in
  let actual = unify empty test_set (Value.var "a") in
  Alcotest.(check int) "unify should return new state with set added to it"
    0 (compare expected actual)

let tests = [
  "test_empty_state", `Quick, test_empty_state;
  "test_to_string_on_empty_state", `Quick, test_to_string_on_empty_state;
  "test_to_string_on_variables", `Quick, test_to_string_on_variables;
  "test_to_string_on_set", `Quick, test_to_string_on_set;
  "test_value_of_on_non_variable", `Quick, test_value_of_on_non_variable;
  "test_value_of_on_non_matching_variable", `Quick, test_value_of_on_non_matching_variable;
  "test_value_of_on_matching_variable", `Quick, test_value_of_on_matching_variable;
  "test_value_of_on_set_with_matching_variable", `Quick, test_value_of_on_set_with_matching_variable;
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