open Core_kernel
open Incr_dom

open Natural_deduction

module Jsoo = Js_of_ocaml

module Model = struct
  type t =
    { proof : Proof.Focused.t }
  [@@deriving sexp, fields, compare]

  (* TODO *)
  let set_default_input () =
    (* { proof = Proof.Focused.of_figure Test_structures.Partial.ex6 } *)
    { proof = Proof.Focused.of_figure
          (Notation.Figure.initial @@ Proof.Partial.Formula.complete Notation.Formula.(prop "A")) }

  let init () = set_default_input ()

  let log_focused_formula (focused : Proof.Focused.t) =
    Jsoo.Firebug.console##log
      (focused.proof
       |> Proof.Zipper.focus
       |> Notation.Figure.endformula
       |> Proof.Partial.Formula.to_string)

  let grow_proof t (proof : Proof.Focused.t) =
    log_focused_formula proof;
    (* TODO rm exceptional code *)
    try match Proof.Focused.explore_tactics proof with
      | Ok proof -> { proof }
      | Error `None       -> t
      | Error `Initial    -> raise (Failure "TODO Handle err: `Initial")
      | Error `Not_a_hole -> raise (Failure "TODO Handle err: `Not_a_hole")
      | Error `Iter_on_noninitial -> raise (Failure "TODO Handle err: `Iter_on_noninitial")
    with Failure msg ->
      Jsoo.Firebug.console##log ("Something went wrong: " ^ msg);
      t

  let apply_tactic t tactic =
    log_focused_formula t.proof;
    try match Proof.Focused.apply_tactic t.proof tactic with
      | Ok proof ->
        Jsoo.Firebug.console##log
          (Proof.Zipper.to_figure proof.proof
           |> Notation.Figure.to_string
             ~formula:Proof.Partial.Formula.to_string
             ~rule:Proof.Partial.Rule.to_string
          );
        log_focused_formula proof;
        { proof = Proof.Focused.clear_tactics proof }
      | Error `None       -> t
      | Error `Initial    -> raise (Failure "TODO Handle err: `Initial")
      | Error `Not_a_hole -> raise (Failure "TODO Handle err: `Not_a_hole")
      | Error `Iter_on_noninitial -> raise (Failure "TODO Handle err: `Iter_on_noninitial")
    with Failure msg ->
      Jsoo.Firebug.console##log ("Something went wrong: " ^ msg);
      t

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = Action

module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ =
  match (action : Action.t) with
  (* TODO *)
  | Grow_proof proof -> Model.grow_proof model proof
  | Apply_tactic tactic -> Model.apply_tactic model tactic

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let view (m : Model.t Incr.t) ~inject =
  let module View = View.Make (struct let inject = inject end) in
  let open Incr.Let_syntax in
  let open Vdom in
  let%map proof =
    let%map proof = m >>| Model.proof in
    Node.div [] [ View.Proof.view proof ]
  in
  Node.div [] [ proof ]

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model in
    apply_action model
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action model view
