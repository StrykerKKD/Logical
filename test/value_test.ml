open Logical.Value

let test_int () =
  let actual = (ValueType.Int 42) = (int 42) in
  Alcotest.(check bool) "int should return correct tagged value" true actual

let test_float () =
  let actual = (ValueType.Float 42.) = (float 42.) in
  Alcotest.(check bool) "float should return correct tagged value" true actual

let test_str () =
  let actual = (ValueType.Str "hello") = (str "hello") in
  Alcotest.(check bool) "str should return correct tagged value" true actual

let test_bool () =
  let actual = (ValueType.Bool true) = (bool true) in
  Alcotest.(check bool) "bool should return correct tagged value" true actual

let test_var () =
  let actual = (ValueType.Var "var") = (var "var") in
  Alcotest.(check bool) "var should return correct tagged value" true actual

let test_set () =
  let test_set = Base.Set.of_list (module ValueComparator) [int 1; int 2; int 3] in
  let expected = "(Set((Int 1)(Int 2)(Int 3)))" in
  let actual = set test_set |> to_string in
  Alcotest.(check string) "set should return correct tagged value" expected actual

let tests = [
  "test_int", `Quick, test_int;
  "test_float", `Quick, test_float;
  "test_str", `Quick, test_str;
  "test_bool", `Quick, test_bool;
  "test_var", `Quick, test_var;
  "test_set", `Quick, test_set;
]