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
    imp f fig >>= fun fig ->
    let conclusion = Figure.endformula fig in
    if not Formula.(equal (def F) conclusion) then
      None
    else
      let open Formula.Infix
      in Option.some @@
      deriv
        [fig]
        (****)
        (!! f)
end

module Elim = struct
  open Figure
  (* open Expression *)

  let conj_left : t -> t option =
    fun fig ->
    match Figure.endformula fig with
    | Comp (And (a, _)) -> Option.some @@
      deriv
        [fig]
        (***)
        a
    | _ -> None

  let conj_right : t -> t option =
    fun fig ->
    match Figure.endformula fig with
    | Comp (And (_, b)) -> Option.some @@
      deriv
        [fig]
        (***)
        b
    | _ -> None

  (* let disj : t -> t -> t -> t option =
   *   fun a_or_b_fig a_imp_c_fig b_imp_c_fig ->
   *   let a_or_b = Figure.endformula a_or_b_fig
   *   and a_imp_c = Figure.endformula a_imp_c_fig
   *   and b_imp_c = Figure.endformula b_imp_c_fig
   *   in *)


end
