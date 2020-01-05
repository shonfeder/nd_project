open! Core_kernel
open Natural_deduction

open Proof

let ex_proof = Option.value_exn Ex_proofs.Ex_1_1.proof

let print_fig fig =
  print_endline "FIGS: ";
  print_endline (Partial.Figure.to_string fig)

let tactics_unit_tests = Helpers.unit_suite "Proof Tactics Unit Tests"
    [
      begin
        let p = Partial.Formula.complete Ex_proofs.Prop.x in
        (* The figure initially has a single initial formula *)
        let fig = Notation.Figure.initial p in
        let () = print_fig fig in
        Helpers.unit "reiter reiterates initial figures in derivation"
          Alcotest.(list Nd_unit.testable_partial_formula)
          (* After reiter, it's expected to have 2 initial formula *)
          ~expected:[p; p]
          ~actual:begin
            Proof.Tactic.apply
              (Zipper.of_figure fig)
              Proof.Tactic.Reiter
            |> Result.ok |> Option.value_exn
            |> (fun z -> Zipper.to_figure z |> print_fig; z)
            |> Zipper.focus
            |> Notation.Figure.initial_formulae
          end
      end;

      (* begin
       *   let p = Partial.Formula.complete Ex_proofs.Prop.x in
       *   (\* The figure initially has a single initial formula *\)
       *   let fig = Notation.Figure.initial p in
       *   let () = print_fig fig in
       *   Helpers.unit "reiter reiterates initial figures in derivation"
       *     Alcotest.(list Nd_unit.testable_partial_formula)
       *     (\* After reiter, it's expected to have 2 initial formula *\)
       *     ~expected:[p; p]
       *     ~actual:begin
       *       Proof.Tactic.apply
       *         (Zipper.of_figure fig)
       *         Proof.Tactic.Reiter
       *       |> Result.ok |> Option.value_exn
       *       |> (fun z -> Zipper.to_figure z |> print_fig; z)
       *       |> Zipper.focus
       *       |> Notation.Figure.initial_formulae
       *     end
       * end; *)

      begin
        let p = Partial.Formula.complete Ex_proofs.Prop.x in
        (* The figure initially has a single initial formula *)
        let fig = Notation.Figure.initial p in
        let () = print_fig fig in
        Helpers.unit "Focused.apply_tactics reiterates initial figures in derivation"
          Alcotest.(list Nd_unit.testable_partial_formula)
          (* After reiter, it's expected to have 2 initial formula *)
          ~expected:[p; p]
          ~actual:begin
            let focused = Proof.Focused.of_figure fig in
            Proof.Focused.apply_tactic
              focused
              Proof.Tactic.Reiter
            |> Result.ok |> Option.value_exn |> (fun Proof.Focused.{proof; _} -> proof)
            |> (fun z -> Zipper.to_figure z |> print_fig; z)
            |> Zipper.focus
            |> Notation.Figure.initial_formulae
          end
      end
    ]
