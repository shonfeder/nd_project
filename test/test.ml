open! Core_kernel

let () =
  Alcotest.run "Tests" [
    Calculus_test.unit_tests
  ];
