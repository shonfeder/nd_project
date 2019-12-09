open Core_kernel

open Notation

module Partial = Proof_partial
module Zipper = Proof_zipper

(* (\** The tactics that can be used to develop a proof *\)
 * module Tactic_t = struct
 *   type t =
 *     | Intro_imp
 *     | Elim_and
 *   [@@deriving sexp, compare, show { with_path = false }]
 *
 *   let to_string = show
 * end
 *
 * module Tactic_aliases = struct
 *   type 'a err =
 *     [> `Initial
 *     |  `Not_a_hole
 *     ] as 'a
 *
 *   type 'err result = (Zipper.partial, 'err err) Result.t
 *   type 'err application = Zipper.partial -> 'err result
 * end *)

include Proof_tactics_types


(* TODO Wow this is gnarly... need to refactor and improve approach *)
exception Impossible
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

let elim_and _z _dir = raise (Failure "TODO")

let apply_rule z (r : Rule.t) = match r.op, r.mode, r.dir with
  | Imp, Intro, None -> intro_imp z
  | And, Elim, (Some dir) -> elim_and z dir
  | _ -> raise (Failure ("TODO: Handle tactic for rule " ^ Rule.to_string r))

(* let open Option.Let_syntax in
 * let x =  *)

let retier _ = raise (Failure "TODO: Handle reiter")

let apply z tac : 'err result = match (tac : t) with
  | Apply_rule r -> apply_rule z r
  | Reiter       -> retier z

(* TODO Insert antecedent ... consequent partial into upper *)
