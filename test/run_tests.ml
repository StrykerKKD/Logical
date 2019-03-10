let test_suites = [
    "Value_test", Value_test.tests;
    "State_test", State_test.tests;
]

let () = Alcotest.run "logical" test_suites