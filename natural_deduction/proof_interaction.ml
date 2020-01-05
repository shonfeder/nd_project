open Core_kernel

module Partial = Proof_partial
module Zipper = Proof_zipper
module Tactic = Proof_tactics

module type Focused = sig
  val possible_tactics : Zipper.partial -> Tactic.t list option
  val using_tactic : Zipper.partial -> Tactic.t -> _ Tactic.result
  val explore_tactics : Zipper.partial -> [> `Applied of _ Tactic.result
                                          | `Options of Tactic.t sexp_list ] option
end

module Focused = struct
  open Notation

  type t =
    { proof: Zipper.partial
    ; tactics: Tactic.t list
    }
  [@@deriving sexp, compare]

  let of_figure : Partial.Figure.t -> t = fun fig ->
    { proof = Zipper.of_figure fig
    ; tactics = []
    }

  let to_figure : t -> Partial.Figure.t =
    fun focused -> focused.proof |> Zipper.to_figure

  let compound_tactics : Formula.compound -> Tactic.t list = function
    | Imp _ ->
      [Apply_rule (Rule.make ~op:Imp ~mode:Intro ()) ]
    | And _ ->
      [ Apply_rule (Rule.make ~op:And ~mode:Elim ~dir:Left ())
      ; Apply_rule (Rule.make ~op:And ~mode:Elim ~dir:Right ())
      ]
    | _ -> raise (Failure "TODO unhandled compound tactic")

  let formula_tactics : Partial.Formula.t -> Tactic.t list = function
    | Complete (Comp cmp) -> compound_tactics cmp
    | _ -> raise (Failure "TODO unhandled possible tactics")

  let possible_tactics z =
    match Zipper.focus z with
    | Initial f        -> Tactic.Reiter :: formula_tactics f
    | (Deriv _ as fig) -> Figure.endformula fig |> formula_tactics

  let apply_tactic t tactic =
    match Tactic.apply t.proof tactic with
    | Error err -> Error err
    | Ok proof -> Ok {t with proof}

  let explore_tactics t =
    match possible_tactics t.proof with
    | [] -> Error `None
    (* If only one tactic is possible, then use it *)
    | [tactic] -> apply_tactic t tactic
    | tactics  -> Ok {t with tactics}

  let clear_tactics t = {t with tactics = []}
end
