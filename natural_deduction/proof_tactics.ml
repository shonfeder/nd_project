open Core_kernel

open Notation

module Partial = Proof_partial
module Zipper = Proof_zipper

include Proof_tactics_types

exception Impossible

(* TODO Wow this is gnarly... need to refactor and improve approach *)
let intro_imp z =
  match Zipper.peek_upper z with
  | None -> Error `Initial
  | Some Partial.[Initial Formula.Hole] ->
    let f fig = match (fig : _ Figure.t) with
      | Initial _ -> raise Impossible
      | Deriv deriv ->
        let partial_imp_intro =
          let (a, b) =
            Option.(value_exn ~message:"Invalid goal for tactic"
                      (Partial.Formula.get_complete deriv.lower >>= Formula.get_imp))
          in
          let ass_a_hole =
            Figure.deriv
              [Figure.assume (Partial.Formula.complete a)]
              (*******************) ~rule:Partial.Rule.hole
              Partial.Formula.hole
          in
          Figure.deriv
            [ass_a_hole]
            (***********) ~rule:Partial.Rule.hole
            (Partial.Formula.complete b)
        in
        let rule = Partial.Rule.promised (Rule.make ~op:Imp ~mode:Intro ()) in
        Figure.deriv
          [partial_imp_intro]
          (*****************) ~rule
          deriv.lower
    in
    Ok (Zipper.map ~f z)
  | Some _ -> Error `Not_a_hole

let elim_and z dir =
  let is_hole lower = match Zipper.focus lower |> Figure.get_lower with
    | None   -> false
    | Some p -> Partial.Formula.is_hole p
  in
  match Zipper.move_down z with
  | None                                -> Error `Not_a_hole
  | Some lower when not (is_hole lower) -> Error `Not_a_hole
  | Some _z' ->
    let _conj_elim = match (dir : Rule.dir) with
      | Left  -> Calculus.Elim.conj_left
      | Right -> Calculus.Elim.conj_right
    in
    raise (Failure "TODO")


let apply_rule z (r : Rule.t) = match r.op, r.mode, r.dir with
  | Imp, Intro, None -> intro_imp z
  | And, Elim, (Some dir) -> elim_and z dir
  | _ -> raise (Failure ("TODO: Handle tactic for rule " ^ Rule.to_string r))

let reiter z : 'err result =
  match Zipper.focus z with
  | Deriv _   -> Error `Iter_on_noninitial
  | Initial _ as i ->
    let holy_initial =
      Figure.deriv
        [i]
        ~rule:Partial.Rule.hole
        Partial.Formula.hole
    in
    let reiterated =
      Figure.deriv
        [holy_initial; holy_initial]
        ~rule:Partial.Rule.hole
        Partial.Formula.hole
    in
    let new_proof = Zipper.insert_focus z reiterated in
    Ok new_proof

let apply z tac : 'err result = match (tac : t) with
  | Apply_rule r -> apply_rule z r
  | Reiter       -> reiter z

(* TODO Insert antecedent ... consequent partial into upper *)
