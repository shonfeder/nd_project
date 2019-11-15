(* TODO Need to model assumptions explicitly? *)
open Core_kernel
open Notation

let mottor = Excerpt.gentzen_nd
    {|We intend now to present a calculus for 'natural' intuitionist derivations.
...
Natural deduction ... does not, in general, start from basic logical
propositions, but rather from {i assumptions} to which logical
deductions are applied.|}
    (`Pg 75)

module Intro = struct
  open Figure
  open Expression

  let conj : t -> t -> t option =
    fun a_fig b_fig ->
    let a = endformula a_fig
    and b = endformula b_fig
    in
    let open Formula.Infix
    in Option.some @@
    deriv
      [a_fig; b_fig]
      (************)
      (a && b)

  let disj_right : t -> Formula.t -> t option =
    fun a_fig b ->
    let a = endformula a_fig in
    let open Formula.Infix
    in Option.some @@
    deriv
      [a_fig]
      (******)
      (a || b)

  let disj_left : Formula.t -> t -> t option =
    fun a b_fig ->
    let b = endformula b_fig in
    let open Formula.Infix
    in Option.some @@
    deriv
      [b_fig]
      (******)
      (a || b)

  (* "An arbitrary number (possibly zero) of formulae of this form, all formally
      identical, may be adjoined to the inference figure as assumption formula."
      (76) *)

  let assumption_formula : t -> Formula.t option =
    fun fig ->
    match Figure.initial_formulae fig with
    | [] -> None
    | assumption :: assumptions ->
      let all_are_formally_identical =
        let f bool a = bool && Formula.equal a assumption in
        List.fold ~f ~init:true assumptions
      in
      Option.some_if all_are_formally_identical assumption

  (* TODO Tests *)
  let imp : Formula.t -> t -> t option =
    fun ass fig ->
    let open Option.Monad_infix in
    assumption_formula fig >>= fun antecedent ->
    if not (Formula.equal ass antecedent) then
      None
    else
      let consequent = Figure.endformula fig in
      let open Formula.Infix
      in Option.some @@
      deriv
        [fig]
        (************************)
        (antecedent => consequent)

  let neg : Formula.t -> t -> t option =
    fun f fig ->
    let open Option.Monad_infix in
    let open Formula.Infix in
    imp f fig >>= fun _implies_false ->
    let conclusion = Figure.endformula fig in
    Option.some_if Formula.(equal (def F) conclusion) begin
      deriv
        [fig]
        (****)
        (!! f)
    end
end

module Elim = struct
  open Figure
  open Expression

  let conj_left : t -> t option =
    fun fig ->
    let open Option.Monad_infix in
    Figure.endformula fig |> Formula.get_and >>| fun (a, _) ->
    deriv
      [fig]
      (***)
      a

  let conj_right : t -> t option =
    fun fig ->
    let open Option.Monad_infix in
    Figure.endformula fig |> Formula.get_and >>| fun (_, b) ->
    deriv
      [fig]
      (***)
      b

  let disj : t -> t -> t -> t option =
    fun a_or_b_fig c_from_a_fig c_from_b_fig ->
    let open Option.Monad_infix in
    Figure.endformula a_or_b_fig |> Formula.get_or >>= fun (a, b) ->
    let c_from_a = Figure.endformula c_from_a_fig
    and c_from_b = Figure.endformula c_from_b_fig
    in
    Option.some_if Formula.(equal c_from_a c_from_b) c_from_a >>= fun c ->
    Intro.imp a c_from_a_fig >>= fun _ ->
    Intro.imp b c_from_b_fig >>| fun _ ->
    deriv
      [a_or_b_fig; c_from_a_fig; c_from_a_fig]
      (**************************************)
      c

  let imp : t -> t -> t option =
    fun a_fig a_imp_b_fig ->
    let open Option.Monad_infix in
    Figure.endformula a_imp_b_fig |> Formula.get_imp >>= fun (a, b) ->
    let a' = Figure.endformula a_fig in
    Option.some_if Formula.(equal a a') begin
      deriv
        [a_fig; a_imp_b_fig]
        (******************)
        b
    end

  let neg : t -> t -> t option =
    fun a_fig neg_a_fig ->
    let open Option.Monad_infix in
    let a = Figure.endformula a_fig
    and neg_a = Figure.endformula neg_a_fig
    in
    Formula.get_not neg_a >>= fun a' ->
    Option.some_if Formula.(equal a a') begin
      deriv
        [a_fig; neg_a_fig]
        (****************)
        Formula.(def F)
    end

  let absurd : t -> Formula.t -> t option =
    fun false_fig d ->
    let f = Figure.endformula false_fig in
    Option.some_if Formula.(equal (def F) f) begin
      deriv
        [false_fig]
        (*********)
        d
    end
end
