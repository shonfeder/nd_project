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

  let init () = set_default_input ()

  let grow_proof t (proof : Proof.Focused.t) =
    Jsoo.Firebug.console##log proof;
    match Proof.Focused.explore_tactics proof with
    | Error `None         -> t
    | Error _err          -> raise (Failure "TODO Handle other user errors")
    | Ok (`Options _)     -> raise (Failure "TODO Handle options")
    | Ok (`Applied proof) -> { proof }

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

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let view (m : Model.t Incr.t) ~inject =
  (* TODO This is quite bad, no? *)
  let module View = View.Make (struct let inject = inject end) in
  let open Incr.Let_syntax in
  let open Vdom in
  let%map proof =
    let%map deriv = m >>| Model.proof in
    Node.div [] [
      View.Derivation_zipper.view deriv;
      (* View.Partial_derivation.view deriv *)
    ]
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
