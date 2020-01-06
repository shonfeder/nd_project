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
    { proof = Proof.Focused.of_figure Test_structures.Partial.ex6 }
  (* { proof = Proof.Focused.of_figure
   *       (Notation.Figure.initial @@ Proof.Partial.Formula.complete Notation.Formula.(prop "A")) } *)

  let init () = set_default_input ()

  let grow_proof t (proof : Proof.Focused.t) =
    Aux.log_proof proof;
    (* TODO rm exceptional code *)
    try match Proof.Focused.explore_tactics proof with
      | Ok proof -> { proof }
      | Error `No_tactics -> t
      | Error `Initial    -> raise (Failure "TODO Handle err: `Initial")
      | Error `Not_a_hole -> raise (Failure "TODO Handle err: `Not_a_hole")
      | Error `Iter_on_noninitial -> raise (Failure "TODO Handle err: `Iter_on_noninitial")
      | Error (`Unknown err) -> raise (Failure ("Unknown error: " ^ err))
    with Failure msg ->
      Aux.log_err ("Something went wrong: " ^ msg);
      t

  let apply_tactic t tactic =
    Aux.log_str "Before...";
    Aux.log_proof t.proof;
    try match Proof.Focused.apply_tactic t.proof tactic with
      | Ok proof ->
        Aux.log_str "After...";
        Aux.log_proof proof;
        { proof = Proof.Focused.clear_tactics proof }
      | Error `No_tactics -> t
      | Error `Initial    -> raise (Failure "TODO Handle err: `Initial")
      | Error `Not_a_hole -> raise (Failure "TODO Handle err: `Not_a_hole")
      | Error `Iter_on_noninitial -> raise (Failure "TODO Handle err: `Iter_on_noninitial")
      | Error (`Unknown err) -> raise (Failure ("Unknown error: " ^ err))
    with Failure msg ->
      Aux.log_err ("Something went wrong: " ^ msg);
      t

  (* let cutoff t1 t2 = compare t1 t2 = 0 *)
  let cutoff _ _ = false
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
