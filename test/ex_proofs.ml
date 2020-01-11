open! Core_kernel
open Natural_deduction
open Notation

module Calculus = Calculus.Make (Proof.Complete)

module Prop = struct
  let x = Formula.prop "X"
  let y = Formula.prop "Y"
  let z = Formula.prop "Z"
end

(** From p. 70 *)
module Ex_1_1 = struct
  open Calculus
  open Prop

  let proves =
    let open Formula.Infix in
    (x || (y && z)) => ((x || y) && (x || z))

  let proof =
    let open Option.Let_syntax in
    let open Formula.Infix
    in
    let x_or_y_and_z = (x || (y && z)) in
    let x_or_y_and_z_ass = Figure.assume x_or_y_and_z
    in
    let%bind x_or_y_and_x_or_z =
      let%bind from_x =
        let x_ass = Figure.assume x
        in
        let%bind x_or_y = Intro.disj_right x_ass y in
        let%bind x_or_z = Intro.disj_right x_ass z in
        Intro.conj x_or_y x_or_z
      in
      let%bind from_x_and_z =
        let y_and_z_ass = Figure.assume (y && z)
        in
        let%bind y_fig = Elim.conj_left y_and_z_ass in
        let%bind z_fig = Elim.conj_right y_and_z_ass in
        let%bind x_or_y = Intro.disj_left x y_fig in
        let%bind x_or_z = Intro.disj_left x z_fig in
        Intro.conj x_or_y x_or_z
      in
      Elim.disj x_or_y_and_z_ass from_x from_x_and_z
    in
    print_endline (Proof.Complete.Figure.to_string x_or_y_and_x_or_z);
    Intro.imp x_or_y_and_z x_or_y_and_x_or_z
end
