open Logical
open Value

let test_int () =
  let actual = compare (Type.Int 42) (int 42) in
  Alcotest.(check int) "int should return correct tagged value" 0 actual

let test_float () =
  let actual = compare (Type.Float 42.) (float 42.) in
  Alcotest.(check int) "float should return correct tagged value" 0 actual

let test_str () =
  let actual = compare (Type.Str "hello") (str "hello") in
  Alcotest.(check int) "str should return correct tagged value" 0 actual

let test_bool () =
  let actual = compare (Type.Bool true) (bool true) in
  Alcotest.(check int) "bool should return correct tagged value" 0 actual

let test_var () =
  let actual = compare (Type.Var "var") (var "var") in
  Alcotest.(check int) "var should return correct tagged value" 0 actual

let tests = [
  "test_int", `Quick, test_int;
  "test_float", `Quick, test_float;
  "test_str", `Quick, test_str;
  "test_bool", `Quick, test_bool;
  "test_var", `Quick, test_var;
]