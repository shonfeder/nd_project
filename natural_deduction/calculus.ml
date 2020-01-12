(* TODO Need to model assumptions explicitly? *)
open Core_kernel
open Notation

let motto = Excerpt.gentzen_nd
    {|We intend now to present a calculus for 'natural' intuitionist derivations.
...
Natural deduction ... does not, in general, start from basic logical
propositions, but rather from {i assumptions} to which logical
deductions are applied.|}
    (`Pg 75)

module Make (P : Proof_intf.S) = struct
  (** [module Calculus = Make (P) is a calculus for proofs which are either
      [Partial] or [Complete], allowing the derivation rules of the calculus to
      apply to proofs of the respective kinds. *)

  type fig = P.Figure.t
  type formula = P.Formula.t

  (* TODO Allow for promised formulas? *)
  (** [get_endformula fig] is [Some f] if [f] is the complete endformula of
      [fig], or else [None], if [f] is a hole*)
  let get_endformula fig = Figure.endformula fig |> P.Formula.to_complete

  (** [deriv formulas ~rule conclusion] is just a derivation formed using
      [Figure.deriv], except the conclusion is wrapped into the representation
      of a [complete] formula provided by [P.Formula.complete] *)
  let deriv formulas ~rule conclusion =
    Figure.deriv
      formulas
      ~rule
      (P.Formula.of_complete conclusion)

  module Intro = struct
    open Option.Let_syntax

    module Infix = Formula.Infix

    let conj : fig -> fig -> fig option =
      fun a_fig b_fig ->
      let%bind a = get_endformula a_fig in
      let%map b = get_endformula b_fig
      in
      let rule = P.Rule.(make ~op:Symbol.And ~mode:Intro ()) in
      deriv
        [a_fig; b_fig]
        (************) ~rule
        Infix.(a && b)

    let disj_right : fig -> formula -> fig option =
      fun a_fig b ->
      let%bind b = P.Formula.to_complete b in
      let%map a = get_endformula a_fig in
      let rule = P.Rule.(make ~op:Symbol.Or ~mode:Intro ~dir:Right ()) in
      deriv
        [a_fig]
        (******) ~rule
        Infix.(a || b)

    let disj_left : formula -> fig -> fig option =
      fun a b_fig ->
      let%bind a = P.Formula.to_complete a in
      let%map b = get_endformula b_fig in
      let rule = P.Rule.(make ~op:Symbol.Or ~mode:Intro ~dir:Left ()) in
      deriv
        [b_fig]
        (******) ~rule
        Infix.(a || b)

    (* TODO Determine for sure whether this is needed *)
    (** "An arbitrary number (possibly zero) of formulae of this form, all
        formally identical, may be adjoined to the inference figure as assumption
        formula." (76) *)
    (* let assumption_formula : fig -> Formula.t option =
     *   fun fig ->
     *   match Figure.initial_formulae fig with
     *   | [] -> None
     *   | assumption :: assumptions ->
     *     let all_are_formally_identical =
     *       let f bool a = bool && Formula.equal a assumption in
     *       List.fold ~f ~init:true assumptions
     *     in
     *     if all_are_formally_identical then
     *       Some assumption
     *     else
     *       (print_endline "Failed at assumption formula";
     *        None) *)

    (* TODO Tests *)
    let imp : P.Formula.t -> fig -> fig option =
      fun antecedent fig ->
      (* let antecedent = P.Formula.to_complete antecedent in *)
      let assumptions = Figure.initial_formulae fig in
      if not (List.mem assumptions antecedent ~equal:P.Formula.equal) then
        None
      else
        let%bind antecedent = P.Formula.to_complete antecedent in
        let%map consequent = get_endformula fig in
        let rule = P.Rule.(make ~op:Symbol.Imp ~mode:Intro ()) in
        deriv
          [fig]
          (************************) ~rule
          Infix.(antecedent => consequent)

    let neg : P.Formula.t -> fig -> fig option =
      fun f fig ->
      let%bind _implies_false = imp f fig in
      let%bind f = P.Formula.to_complete f in
      let%bind conclusion = get_endformula fig in
      let rule = P.Rule.(make ~op:Symbol.Not ~mode:Intro ()) in
      Option.some_if Formula.(equal (def F) conclusion) begin
        deriv
          [fig]
          (****) ~rule
          Infix.(!! f)
      end
  end

  module Elim = struct
    open Option.Let_syntax

    let conj_left : fig -> fig option =
      fun fig ->
      let%map (a, _) = get_endformula fig >>= Formula.get_and in
      let rule = P.Rule.(make ~op:Symbol.And ~mode:Elim ~dir:Left ()) in
      deriv
        [fig]
        (***) ~rule
        a

    let conj_right : fig -> fig option =
      fun fig ->
      let%map (_, b) = get_endformula fig >>= Formula.get_and  in
      let rule = P.Rule.(make ~op:Symbol.And ~mode:Elim ~dir:Right ()) in
      deriv
        [fig]
        (***) ~rule
        b

    let disj : fig -> fig -> fig -> fig option =
      fun a_or_b_fig c_from_a_fig c_from_b_fig ->
      let%bind (a, b) = get_endformula a_or_b_fig >>= Formula.get_or in
      let%bind c_from_a = get_endformula c_from_a_fig in
      let%bind c_from_b = get_endformula c_from_b_fig
      in
      let%bind c = Option.some_if Formula.(equal c_from_a c_from_b) c_from_a in
      let%bind _a_implies_c = Intro.imp (P.Formula.of_complete a) c_from_a_fig in
      let%map  _b_implies_c = Intro.imp (P.Formula.of_complete b) c_from_b_fig in
      let rule = P.Rule.(make ~op:Symbol.Or ~mode:Elim ()) in
      deriv
        [a_or_b_fig; c_from_a_fig; c_from_a_fig]
        (**************************************) ~rule
        c

    let imp : fig -> fig -> fig option =
      fun a_fig a_imp_b_fig ->
      let%bind (a, b) = get_endformula a_imp_b_fig >>= Formula.get_imp in
      let%bind a' = get_endformula a_fig in
      Option.some_if Formula.(equal a a') begin
        let rule = P.Rule.(make ~op:Symbol.Imp ~mode:Elim ()) in
        deriv
          [a_fig; a_imp_b_fig]
          (******************) ~rule
          b
      end

    let neg : fig -> fig -> fig option =
      fun a_fig neg_a_fig ->
      let%bind a = get_endformula a_fig in
      let%bind neg_a = get_endformula neg_a_fig
      in
      let%bind a' = Formula.get_not neg_a in
      Option.some_if Formula.(equal a a') begin
        let rule = P.Rule.(make ~op:Symbol.Not ~mode:Elim ()) in
        deriv
          [a_fig; neg_a_fig]
          (****************) ~rule
          Formula.(def F)
      end

    let absurd : fig -> formula -> fig option =
      fun false_fig d ->
      let%bind d = P.Formula.to_complete d in
      let%bind f = get_endformula false_fig in
      Option.some_if Formula.(equal (def F) f) begin
        let rule = P.Rule.(make ~op:Symbol.Explode ~mode:Elim ()) in
        deriv
          [false_fig]
          (*********) ~rule
          d
      end
  end
end (* Make *)
