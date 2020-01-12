open! Core_kernel

let () =
  Alcotest.run "Tests"
    [ Calculus_test.Complete.unit_tests
    ; Calculus_test.Partial.unit_tests
    ; Proof_zipper_test.zipper_unit_tests
    ; Tactics_test.tactics_unit_tests
    ];
