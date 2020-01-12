open! Core_kernel
open Natural_deduction
open Notation

let testable_formula =
  let pp = Fmt.of_to_string Formula.to_string in
  Alcotest.testable pp Formula.equal

module Make (P : Proof.S) = struct
  let figure =
    let pp = Fmt.of_to_string P.Figure.to_string in
    let equal = P.Figure.equal
    in
    Alcotest.testable pp equal

  let formula =
    let pp = Fmt.of_to_string P.Formula.to_string in
    Alcotest.testable pp P.Formula.equal

  (** True when [deriv] is a derivations of [concludes] *)
  let test_deriv name ~concludes deriv =
    let actual = Option.map ~f:Figure.endformula deriv in
    Helpers.unit name (Alcotest.option formula) ~expected:(Some concludes) ~actual

  let test_upper name ~expected ~actual =
    Helpers.unit name (Alcotest.list figure) ~expected ~actual

  let test_figure name ~expected ~actual =
    Helpers.unit name (Alcotest.option figure) ~expected ~actual
end (* Proof *)

module Partial  = Make (Proof.Partial)
module Complete = Make (Proof.Complete)
