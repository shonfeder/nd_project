open! Core_kernel
open Natural_deduction
open Notation

let testable_formula =
  let pp = Fmt.of_to_string Formula.to_string in
  Alcotest.testable pp Formula.equal

let testable_figure =
  let pp =
    let to_string =
      let formula = Formula.to_string in
      let rule = Proof.Complete.Rule.to_string in
      Figure.to_string ~formula ~rule
    in
    Fmt.of_to_string to_string
  in
  let equal =
    let formula = Formula.compare in
    let rule = Proof.Complete.Rule.compare in
    Figure.equal ~formula ~rule
  in
  Alcotest.testable pp equal

(** True when [deriv] is a derivations of [concludes] *)
let test_deriv name ~concludes deriv =
  let actual = Option.map ~f:Figure.endformula deriv in
  Helpers.unit name (Alcotest.option testable_formula) ~expected:(Some concludes) ~actual

let test_upper name ~expected ~actual =
  Helpers.unit name (Alcotest.list testable_figure) ~expected ~actual

let test_figure name ~expected ~actual =
  Helpers.unit name (Alcotest.option testable_figure) ~expected ~actual
