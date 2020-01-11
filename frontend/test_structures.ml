(* tmp test structures used to populate front end *)

open Core_kernel
open Natural_deduction
open Notation

module Calculus = Calculus.Make (Proof.Complete)

let derivation =
  let open Option.Let_syntax in
  let a = Formula.(not_ (prop "A"))
  and b = Formula.prop "B" |> Figure.assume
  and c = Formula.prop "C"
  and d = (Formula.prop "D")
  in
  let open Calculus in
  let deriv =
    let%bind a_or_d = Calculus.Intro.disj_right (Figure.assume a) d in
    let%bind a_imp_a_or_d = Intro.imp a a_or_d in
    let%bind b_or_d = Intro.disj_right b d in
    let%bind a_and_b_or_d = Intro.conj a_imp_a_or_d b_or_d in
    let%map c_or_a_and_b_or_d = Intro.disj_left c a_and_b_or_d in
    c_or_a_and_b_or_d
  in
  (* TODO *)
  Option.value_exn deriv


module Partial = struct

  open Proof

  let a_prop = Formula.prop "A"
  let b_prop = Formula.prop "B"

  let ex =
    (* ⋮    ⋮
     * A   B
     * ————— ∧I
     * A ∧ B *)
    let a =
      Figure.deriv
        []
        (Partial.Formula.complete a_prop)
        ~rule:Partial.Rule.hole
    in
    let b  =
      Figure.deriv
        []
        (Partial.Formula.complete b_prop)
        ~rule:Partial.Rule.hole
    in
    Figure.{ upper = [a; b]
           ; lower = Partial.Formula.complete Formula.Infix.(a_prop && b_prop)
           ; rule  = Partial.Rule.complete Complete.Rule.(make ~op:Symbol.And ~mode:Intro ())
           }

  let ex2 =
    (*     ⋮
     * ————————— ∨I_
     *   A ∨ B *)
    Figure.deriv []
      (Partial.Formula.complete Formula.Infix.(a_prop || b_prop))
      ~rule:(Partial.Rule.promised Complete.Rule.(make ~op:Symbol.Or ~mode:Intro ()))

  let ex3 =
    (*  |A|
     *   ⋮
     *   B
     * ————— →I
     * A → B *)
    let assum = (Figure.assume (Partial.Formula.complete a_prop)) in
    let deriv_b =
      Figure.deriv
        [assum]
        (Partial.Formula.complete b_prop)
        ~rule:Partial.Rule.hole
    in
    Figure.deriv
      [deriv_b]
      (Partial.Formula.complete Formula.Infix.(a_prop => b_prop))
      ~rule:Partial.Rule.(complete (Complete.Rule.(make ~op:Symbol.Imp ~mode:Intro ())))

  let a_and_a_imp_b_prop = Formula.Infix.(a_prop && (a_prop => b_prop))

  let ex4 =
    (*  |A ∧ A → B|
     *       ⋮
     *  ———————————
     *       B
     * ——————————————— →I
     * (A ∧ A → B) → B *)
    let assum = Figure.assume Partial.Formula.(complete a_and_a_imp_b_prop) in
    let upper =
      Figure.deriv
        [assum]
        (Partial.Formula.complete b_prop)
        ~rule:Partial.Rule.hole
    in
    Figure.deriv
      [upper]
      (Partial.Formula.complete a_and_a_imp_b_prop)
      ~rule:(Partial.Rule.complete (Complete.Rule.(make ~op:Symbol.Imp ~mode:Intro ())))

  let ex5 =
    (* |A ∧ A → B|   |A ∧ A → B|
     *       ⋮             ⋮
     * —————————————————————————— _
     *             B
     *      ——————————————— →I
     *      (A ∧ A → B) → B *)
    let assum = (Figure.assume (Partial.Formula.complete a_and_a_imp_b_prop)) in
    let deriv_from_assum =
      Figure.deriv
        [assum]
        Partial.Formula.hole
        ~rule:Partial.Rule.hole
    in
    let deriv_of_b =
      Figure.deriv
        [deriv_from_assum; deriv_from_assum]
        (Partial.Formula.complete b_prop)
        ~rule:(Partial.Rule.hole)
    in
    Figure.deriv
      [deriv_of_b]
      (Partial.Formula.complete Formula.Infix.(a_and_a_imp_b_prop => b_prop))
      ~rule:(Partial.Rule.complete (Complete.Rule.(make ~op:Symbol.Imp ~mode:Intro ())))

  let ex6 =
    Figure.deriv
      [Figure.Initial Partial.Formula.Hole]
      (Partial.Formula.complete Formula.Infix.(a_and_a_imp_b_prop => b_prop))
      ~rule:(Partial.Rule.complete (Complete.Rule.(make ~op:Symbol.Imp ~mode:Intro ())))

end
