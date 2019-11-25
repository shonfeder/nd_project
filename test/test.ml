open! Core_kernel

let () =
  Alcotest.run "Tests" [
    Calculus.unit_tests
  ];
