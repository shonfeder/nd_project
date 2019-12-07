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

  let compound_tactics : Formula.compound -> Tactic.t list option = function
    | Imp _ -> Some [Intro_imp]
    | And _ -> Some [Elim_and]
    | _ -> raise (Failure "TODO")

  let possible_tactics z =
    match (Zipper.focus z |> Figure.endformula : Partial.Formula.t) with
    | Complete (Comp cmp) -> compound_tactics cmp
    | _ -> raise (Failure "TODO")

  let explore_tactics {proof; tactics} = match possible_tactics proof with
    | None -> Error `None
    (* If only one tactic is possible, then use it *)
    | Some [tactic] -> begin
        match Tactic.apply proof tactic with
        | Error err -> Error err
        | Ok proof -> Ok (`Applied ({proof; tactics}))
      end
    | Some tactics  -> Ok (`Options {proof; tactics})
end
