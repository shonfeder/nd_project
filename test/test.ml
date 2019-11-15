open Core_kernel
open Natural_deduction

let property name = QCheck.Test.make ~name

let testable_formula =
  let pp = Fmt.of_to_string Notation.Expression.Formula.to_string in
  Alcotest.testable pp Notation.Expression.Formula.equal

let unit name ~expected ~actual testable =
  Alcotest.test_case name `Quick
    begin fun () ->
      Alcotest.check testable "" expected actual
    end

let test name tested =
  Alcotest.test_case name `Quick
    begin fun () ->
      Alcotest.check Alcotest.bool "true" true tested
    end

let property_suite name tests =
  let tests = List.map ~f:QCheck_alcotest.to_alcotest tests in
  (name, tests)

let unit_suite name tests = (name, tests)

(** True when [deriv] is a derivations of [concludes] *)
let test_deriv name ~concludes deriv =
  let actual = Option.map ~f:Notation.Figure.endformula deriv in
  unit name (Alcotest.option testable_formula) ~expected:(Some concludes) ~actual

let calculus_unit_tests =
  let open Calculus in
  let open Notation in
  let open Expression in
  let open Option.Let_syntax
  in
  let a_prop = Formula.prop "A"
  and b_prop = Formula.prop "B"
  and c_prop = Formula.prop "C"
  in
  let a = Figure.assume a_prop
  and b = Figure.assume b_prop
  in
  unit_suite "Calculus Unit Tests"
    [
      test_deriv "conjunction intro"
        (Intro.conj a b)
        ~concludes:Formula.(a_prop && b_prop)
      ;

      test_deriv "disjunction intro right"
        (Intro.disj_right b a_prop)
        ~concludes:Formula.(b_prop || a_prop)
      ;

      test_deriv "disjunction intro left"
        (Intro.disj_left a_prop b)
        ~concludes:Formula.(a_prop || b_prop)
      ;

      test_deriv "imp intro"
        begin
          let%bind a_or_b_deriv = Intro.disj_right a b_prop in
          Intro.imp a_prop a_or_b_deriv
        end
        ~concludes:Formula.(a_prop => (a_prop || b_prop))
      ;

      begin
        let a_and_a_imp_false_prop = Formula.(a_prop && (a_prop => Formula.(def F))) in
        test_deriv "neg intro"
          begin
            let a_and_a_imp_false = a_and_a_imp_false_prop |> Figure.assume in
            let%bind a = Elim.conj_left a_and_a_imp_false in
            let%bind a_imp_false = Elim.conj_right a_and_a_imp_false in
            let%bind deriv_false = Elim.imp a a_imp_false in
            Intro.neg a_and_a_imp_false_prop deriv_false
          end
          ~concludes:Formula.(!! a_and_a_imp_false_prop)
      end;

      test_deriv "conj elim left"
        begin
          let a_and_b = Formula.(a_prop && b_prop) |> Figure.assume in
          Elim.(conj_left a_and_b)
        end
        ~concludes:a_prop
      ;

      test_deriv "conj elim right"
        begin
          let a_and_b = Formula.(a_prop && b_prop) |> Figure.assume in
          Elim.(conj_right a_and_b)
        end
        ~concludes:b_prop
      ;

      test_deriv "disj elim"
        begin
          let a_and_c = Formula.(a_prop && c_prop)
          and b_and_c = Formula.(b_prop && c_prop)
          in
          let a_and_c_or_b_and_c = Formula.(a_and_c || b_and_c) |> Figure.assume
          in
          let%bind c_from_a_and_c = Figure.assume a_and_c |> Elim.conj_right in
          let%bind c_from_b_and_c = Figure.assume b_and_c |> Elim.conj_right
          in
          Elim.disj a_and_c_or_b_and_c c_from_a_and_c c_from_b_and_c
        end
        ~concludes:c_prop
      ;

      test_deriv "imp elim"
        begin
          let a_imp_b = Formula.(a_prop => b_prop) |> Figure.assume in
          Elim.imp a a_imp_b
        end
        ~concludes:b_prop
      ;

      test_deriv "neg elim"
        begin
          let not_a = Formula.(!! a_prop) |> Figure.assume in
          Elim.neg a not_a
        end
        ~concludes:Formula.(def F)
      ;

      test_deriv "absurd elim"
        begin
          let absurd = Formula.(def F) |> Figure.assume in
          Elim.absurd absurd c_prop
        end
        ~concludes:c_prop
      ;
    ]

let () =
  Alcotest.run "Tests" [
    calculus_unit_tests
  ];