let test_suites = [
    "State_test", State_test.tests;
]

let () = Alcotest.run "logical" test_suites