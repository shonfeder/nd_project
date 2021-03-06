open Core_kernel
open Natural_deduction
open Helpers

open Notation
open Option.Let_syntax

module Make (P : Proof.S) = struct

  module Calculus = Calculus.Make (P)
  module Nd_unit = Nd_unit.Make (P)

  open Calculus

  let unit_tests =
    let a_prop = P.Formula.prop "A"
    and b_prop = P.Formula.prop "B"
    and c_prop = P.Formula.prop "C"
    in
    let a = Figure.assume a_prop
    and b = Figure.assume b_prop
    in
    unit_suite "Calculus Unit Tests"
      [
        Nd_unit.test_deriv "conjunction intro"
          (Intro.conj a b)
          ~concludes:P.Formula.Infix_exn.(a_prop && b_prop)
        ;

        Nd_unit.test_deriv "disjunction intro right"
          (Intro.disj_right b a_prop)
          ~concludes:P.Formula.Infix_exn.(b_prop || a_prop)
        ;

        Nd_unit.test_deriv "disjunction intro left"
          (Intro.disj_left a_prop b)
          ~concludes:P.Formula.Infix_exn.(a_prop || b_prop)
        ;

        Nd_unit.test_deriv "imp intro"
          begin
            let%bind a_or_b_deriv = Intro.disj_right a b_prop in
            Intro.imp a_prop a_or_b_deriv
          end
          ~concludes:P.Formula.Infix_exn.(a_prop => (a_prop || b_prop))
        ;

        begin
          let falsum = Formula.def F |> P.Formula.of_complete in
          let a_and_a_imp_false_prop = P.Formula.Infix_exn.(a_prop && (a_prop => falsum)) in
          Nd_unit.test_deriv "neg intro"
            begin
              let a_and_a_imp_false = a_and_a_imp_false_prop |> Figure.assume in
              let%bind a = Elim.conj_left a_and_a_imp_false in
              let%bind a_imp_false = Elim.conj_right a_and_a_imp_false in
              let%bind deriv_false = Elim.imp a a_imp_false in
              Intro.neg a_and_a_imp_false_prop deriv_false
            end
            ~concludes:P.Formula.Infix_exn.(!! a_and_a_imp_false_prop)
        end;

        Nd_unit.test_deriv "conj elim left"
          begin
            let a_and_b = P.Formula.Infix_exn.(a_prop && b_prop) |> Figure.assume in
            Elim.(conj_left a_and_b)
          end
          ~concludes:a_prop
        ;

        Nd_unit.test_deriv "conj elim right"
          begin
            let a_and_b = P.Formula.Infix_exn.(a_prop && b_prop) |> Figure.assume in
            Elim.(conj_right a_and_b)
          end
          ~concludes:b_prop
        ;

        Nd_unit.test_deriv "disj elim"
          begin
            let a_and_c = P.Formula.Infix_exn.(a_prop && c_prop)
            and b_and_c = P.Formula.Infix_exn.(b_prop && c_prop)
            in
            let a_and_c_or_b_and_c = P.Formula.Infix_exn.(a_and_c || b_and_c) |> Figure.assume
            in
            let%bind c_from_a_and_c = Figure.assume a_and_c |> Elim.conj_right in
            let%bind c_from_b_and_c = Figure.assume b_and_c |> Elim.conj_right
            in
            Elim.disj a_and_c_or_b_and_c c_from_a_and_c c_from_b_and_c
          end
          ~concludes:c_prop
        ;

        Nd_unit.test_deriv "imp elim"
          begin
            let a_imp_b = P.Formula.Infix_exn.(a_prop => b_prop) |> Figure.assume in
            Elim.imp a a_imp_b
          end
          ~concludes:b_prop
        ;

        Nd_unit.test_deriv "neg elim"
          begin
            let not_a = P.Formula.Infix_exn.(!! a_prop) |> Figure.assume in
            Elim.neg a not_a
          end
          ~concludes:(Formula.(def F) |> P.Formula.of_complete)
        ;

        Nd_unit.test_deriv "absurd elim"
          begin
            let absurd = Formula.(def F) |> P.Formula.of_complete |> Figure.assume in
            Elim.absurd absurd c_prop
          end
          ~concludes:c_prop
        ;
        Nd_unit.test_deriv "Gentzen's Example 1.1"
          (Ex_proofs.Ex_1_1.proof |> Option.map ~f:P.Figure.of_complete)
          ~concludes:(Ex_proofs.Ex_1_1.proves |> P.Formula.of_complete)
        ;
      ]

end (* Make *)

module Complete = Make (Proof.Complete)
module Partial = Make (Proof.Partial)
