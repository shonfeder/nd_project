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

type fig = Proof.Complete.t

module Intro = struct
  open Figure
  open Expression

  open Option.Let_syntax

  let conj : fig -> fig -> fig option =
    fun a_fig b_fig ->
    let a = endformula a_fig
    and b = endformula b_fig
    in
    let open Formula.Infix in
    let rule = Rule.(make ~op:Symbol.And ~mode:Intro ()) in
    Option.some @@ deriv
      [a_fig; b_fig]
      (************) ~rule
      (a && b)

  let disj_right : fig -> Formula.t -> fig option =
    fun a_fig b ->
    let a = endformula a_fig in
    let open Formula.Infix in
    let rule = Rule.(make ~op:Symbol.Or ~mode:Intro ~dir:Right ()) in
    Option.some @@ deriv
      [a_fig]
      (******) ~rule
      (a || b)

  let disj_left : Formula.t -> fig -> fig option =
    fun a b_fig ->
    let b = endformula b_fig in
    let open Formula.Infix in
    let rule = Rule.(make ~op:Symbol.Or ~mode:Intro ~dir:Left ()) in
    Option.some @@ deriv
      [b_fig]
      (******) ~rule
      (a || b)

  (** "An arbitrary number (possibly zero) of formulae of this form, all
      formally identical, may be adjoined to the inference figure as assumption
      formula." (76) *)
  let assumption_formula : fig -> Formula.t option =
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
  let imp : Formula.t -> fig -> fig option =
    fun ass fig ->
    let%bind antecedent = assumption_formula fig in
    if not (Formula.equal ass antecedent) then
      None
    else
      let consequent = Figure.endformula fig in
      let open Formula.Infix in
      let rule = Rule.(make ~op:Symbol.Imp ~mode:Intro ()) in
      Option.some @@ deriv
        [fig]
        (************************) ~rule
        (antecedent => consequent)

  let neg : Formula.t -> fig -> fig option =
    fun f fig ->
    let open Formula.Infix in
    let%bind _implies_false = imp f fig in
    let conclusion = Figure.endformula fig in
    let rule = Rule.(make ~op:Symbol.Not ~mode:Intro ()) in
    Option.some_if Formula.(equal (def F) conclusion) begin
      deriv
        [fig]
        (****) ~rule
        (!! f)
    end
end

module Elim = struct
  open Figure
  open Expression
  open Option.Let_syntax

  let conj_left : fig -> fig option =
    fun fig ->
    let%map (a, _) = Figure.endformula fig |> Formula.get_and in
    let rule = Rule.(make ~op:Symbol.And ~mode:Elim ~dir:Left ()) in
    deriv
      [fig]
      (***) ~rule
      a

  let conj_right : fig -> fig option =
    fun fig ->
    let%map (_, b) = Figure.endformula fig |> Formula.get_and  in
    let rule = Rule.(make ~op:Symbol.And ~mode:Elim ~dir:Right ()) in
    deriv
      [fig]
      (***) ~rule
      b

  let disj : fig -> fig -> fig -> fig option =
    fun a_or_b_fig c_from_a_fig c_from_b_fig ->
    let%bind (a, b) = Figure.endformula a_or_b_fig |> Formula.get_or in
    let c_from_a = Figure.endformula c_from_a_fig
    and c_from_b = Figure.endformula c_from_b_fig
    in
    let%bind c = Option.some_if Formula.(equal c_from_a c_from_b) c_from_a in
    let%bind _a_implies_c = Intro.imp a c_from_a_fig in
    let%map  _b_implies_c = Intro.imp b c_from_b_fig in
    let rule = Rule.(make ~op:Symbol.Or ~mode:Elim ()) in
    deriv
      [a_or_b_fig; c_from_a_fig; c_from_a_fig]
      (**************************************) ~rule
      c

  let imp : fig -> fig -> fig option =
    fun a_fig a_imp_b_fig ->
    let%bind (a, b) = Figure.endformula a_imp_b_fig |> Formula.get_imp in
    let a' = Figure.endformula a_fig in
    Option.some_if Formula.(equal a a') begin
      let rule = Rule.(make ~op:Symbol.Imp ~mode:Elim ()) in
      deriv
        [a_fig; a_imp_b_fig]
        (******************) ~rule
        b
    end

  let neg : fig -> fig -> fig option =
    fun a_fig neg_a_fig ->
    let a = Figure.endformula a_fig
    and neg_a = Figure.endformula neg_a_fig
    in
    let%bind a' = Formula.get_not neg_a in
    Option.some_if Formula.(equal a a') begin
      let rule = Rule.(make ~op:Symbol.Not ~mode:Elim ()) in
      deriv
        [a_fig; neg_a_fig]
        (****************) ~rule
        Formula.(def F)
    end

  let absurd : fig -> Formula.t -> fig option =
    fun false_fig d ->
    let f = Figure.endformula false_fig in
    Option.some_if Formula.(equal (def F) f) begin
      let rule = Rule.(make ~op:Symbol.Explode ~mode:Elim ()) in
      deriv
        [false_fig]
        (*********) ~rule
        d
    end
end
