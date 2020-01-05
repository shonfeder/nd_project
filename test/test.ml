open! Core_kernel

let () =
  Alcotest.run "Tests"
    [ Calculus_test.unit_tests
    ; Proof_test.zipper_unit_tests
    ; Tactics_test.tactics_unit_tests
    ];
