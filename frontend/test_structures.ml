(* tmp test structures used to populate front end *)

open Core_kernel
open Natural_deduction
open Notation

let derivation =
  let open Option.Let_syntax in
  let a = Expression.Formula.(not_ (prop "A"))
  and b = Expression.Formula.prop "B" |> Figure.assume
  and c = Expression.Formula.prop "C"
  and d = (Expression.Formula.prop "D")
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
  open Notation.Expression
  open Proving.Partial

  let a_prop = Formula.prop "A"
  let b_prop = Formula.prop "B"

  let ex =
    (* ⋮    ⋮
     * A   B
     * ————— ∧I
     * A ∧ B *)
    let a =
      Partial { upper = None
              ; lower = Lower.formula a_prop
              ; rule  = Rule.hole
              }
    in
    let b =
      Partial { upper = None
              ; lower = Lower.formula b_prop
              ; rule  = Rule.hole
              }
    in
    Partial { upper = Some [a; b]
            ; lower = Lower.formula Formula.Infix.(a_prop && b_prop)
            ; rule  = Rule.rule Figure.Rule.(make ~op:Symbol.And ~mode:Intro ())
            }

  let ex2 =
    (*     ⋮
     * ————————— ∨I_
     *   A ∨ B *)
    Partial { upper = None
            ; lower = Lower.formula Formula.Infix.(a_prop || b_prop)
            ; rule  = Rule.partial Figure.Rule.(make ~op:Symbol.Or ~mode:Intro ())
            }

  let ex3 =
    (*  |A|
     *   ⋮
     *   B
     * ————— →I
     * A → B *)
    let assum = Complete (Figure.assume a_prop) in
    let deriv_b =
      Partial { upper = Some [assum]
              ; lower = Lower.formula b_prop
              ; rule  = Rule.hole
              }
    in
    Partial { upper = Some [deriv_b]
            ; lower = Lower.formula Formula.Infix.(a_prop => b_prop)
            ; rule = Rule.rule (Figure.Rule.(make ~op:Symbol.Imp ~mode:Intro ()))
            }

  let a_and_a_imp_b_prop = Formula.Infix.(a_prop && (a_prop => b_prop))

  let ex4 =
    (*  |A ∧ A → B|
     *       ⋮
     *  ———————————
     *       B
     * ——————————————— →I
     * (A ∧ A → B) → B *)
    let assum = Complete (Figure.assume a_and_a_imp_b_prop) in
    let upper =
      Partial { upper = Some [assum]
              ; lower = Lower.formula b_prop
              ; rule  = Rule.hole
              }
    in
    Partial { upper = Some [upper]
            ; lower = Lower.formula a_and_a_imp_b_prop
            ; rule  = Rule.rule (Figure.Rule.(make ~op:Symbol.Imp ~mode:Intro ()))
            }

  let ex5 =
    (* |A ∧ A → B|   |A ∧ A → B|
     *       ⋮             ⋮
     * —————————————————————————— _
     *             B
     *      ——————————————— →I
     *      (A ∧ A → B) → B *)
    let assum = Complete (Figure.assume a_and_a_imp_b_prop) in
    let deriv_from_assum =
      Partial { upper = Some [assum]
              ; lower = Lower.hole
              ; rule  = Rule.hole
              }
    in
    let deriv_of_b =
      Partial { upper = Some [deriv_from_assum; deriv_from_assum]
              ; lower = Lower.formula b_prop
              ; rule  = Rule.hole
              }
    in
    Partial { upper = Some [deriv_of_b]
            ; lower = Lower.formula Formula.Infix.(a_and_a_imp_b_prop => b_prop)
            ; rule  = Rule.rule (Figure.Rule.(make ~op:Symbol.Imp ~mode:Intro ()))}
end
