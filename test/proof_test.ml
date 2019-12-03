open! Core_kernel
open Natural_deduction

open Option.Monad_infix
module Zipper = Proof.Zipper

let ex_proof = Option.value_exn Ex_proofs.Ex_1_1.proof

let zipper_unit_tests = Helpers.unit_suite "Proof Zipper Unit Tests"
    [
      begin
        let name = "initial focus of proof is the entire proof" in
        let actual =
          let f x = Zipper.(x |> of_figure |> focus) in
          Option.map ~f Ex_proofs.Ex_1_1.proof
        in
        Nd_unit.test_figure name
          ~expected:Ex_proofs.Ex_1_1.proof
          ~actual
      end;

      Nd_unit.test_figure "moving down on terminal proof is None"
        ~expected:None
        ~actual:Zipper.(Option.map ~f:of_figure Ex_proofs.Ex_1_1.proof >>= move_down >>| focus)
      ;

      Nd_unit.test_figure "moving left on terminal proof is None"
        ~expected:None
        ~actual:Zipper.(Option.map ~f:of_figure Ex_proofs.Ex_1_1.proof >>= move_left >>| focus)
      ;

      Nd_unit.test_figure "moving right on terminal proof is None"
        ~expected:None
        ~actual:Zipper.(Option.map ~f:of_figure Ex_proofs.Ex_1_1.proof >>= move_right >>| focus)
      ;

      begin
        let name = "moving up in proof yields leftmost figure of upper" in
        let expected = match ex_proof with
          | (Deriv {upper = u :: _; _}) -> Some u
          | _ -> None
        in
        let actual =
          let open Zipper in
          of_figure ex_proof
          |> move_up
          |> Option.map ~f:focus
        in
        Nd_unit.test_figure name ~expected ~actual
      end;

      begin
        let name = "moving up, up, right in proof yields second from left figure of upper's upper" in
        let expected = match ex_proof with
          | (Deriv {upper = u :: _; _}) ->
            begin match u with
              | Deriv {upper = _ :: u :: _; _} -> Some u
              | _ -> None
            end
          | _ -> None
        in
        let actual =
          Zipper.(of_figure ex_proof |> move_up >>= move_up >>= move_right >>| focus)
        in
        Nd_unit.test_figure name ~expected ~actual
      end;

      begin
        let name = "reversing course returns to original position" in
        let expected = Some ex_proof
        in
        let actual =
          Zipper.(of_figure ex_proof |>
                  move_up >>= move_up >>=
                  move_right >>=

                  move_left >>=
                  move_down >>= move_down >>|
                  focus)
        in
        Nd_unit.test_figure name ~expected ~actual
      end;

    ]
