open Core_kernel
open Incr_dom

module Jsoo = Js_of_ocaml

module Model = struct
  type t =
    { counter : int
    ; input_text : string
    ; submitted_text : string option
    }
  [@@deriving sexp, fields, compare]

  let set_default_input counter submitted_text =
    { counter; input_text = sprintf "Default #%d" counter; submitted_text }


  let init () = set_default_input 0 None
  let reset_counter t = set_default_input 0 t.submitted_text
  let incr_counter t = set_default_input (t.counter + 1) t.submitted_text
  let update_input t input_text = { t with input_text }
  let submit_input t = { t with submitted_text = Some t.input_text }
  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t =
    | Reset_counter
    | Incr_counter
    | Update_input of string
    | Submit_input
  [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ =
  match (action : Action.t) with
  | Reset_counter -> Model.reset_counter model
  | Incr_counter -> Model.incr_counter model
  | Update_input text -> Model.update_input model text
  | Submit_input -> Model.submit_input model


let on_startup ~schedule_action:_ _ = Async_kernel.return ()

(* let view (m : Model.t Incr.t) ~inject =
 *   let open Incr.Let_syntax in
 *   let open Vdom in
 *   let button label action =
 *     Node.button [ Attr.on_click (fun _ev -> inject action) ] [ Node.text label ]
 *   in
 *   let submit_button = button "Submit" Action.Submit_input in
 *   let reset_button = button "Reset" Action.Reset_counter in
 *   let incr_button = button "Increment" Action.Incr_counter in
 *   let%map input =
 *     let%map input_text = m >>| Model.input_text in
 *     Node.input
 *       [ Attr.type_ "text"
 *       (\* The value property controls the current value of the text input, whereas the
 *          value attribute only controls its initial value. *\)
 *       ; Attr.string_property "value" input_text
 *       (\* We must update our model with the user's input to keep the virtual dom consistent
 *          with the actual dom. *\)
 *       ; Attr.on_input (fun _ev text -> inject (Action.Update_input text))
 *       ]
 *       []
 *   and submission =
 *     let%map submitted_text = m >>| Model.submitted_text in
 *     let text =
 *       match submitted_text with
 *       | None -> "No submissions yet"
 *       | Some text -> "Your latest submission was: " ^ text
 *     in
 *     Node.div [] [ Node.text text ]
 *   in
 *   Node.body [] [ submission; input; submit_button; reset_button; incr_button ] *)


let view (m : Model.t Incr.t) ~inject =
  let open Incr.Let_syntax in
  let open Vdom in
  let button label action =
    Node.button [ Attr.on_click (fun _ev -> inject action) ] [ Node.text label ]
  in
  let submit_button = button "Submit" Action.Submit_input in
  let reset_button = button "Reset" Action.Reset_counter in
  let incr_button = button "Increment" Action.Incr_counter
  in
  (* ---- *)
  let position ?(attrs=[]) ?(content=[]) () =
    (* TODO refactor out *)
    let style = Attr.style @@ Css_gen.(
        border ~style:`Solid () @>
        height (`Px 60) @>
        margin_top (`Px 20)
      )
    in
    let ondrop = Attr.on "drop" (fun _ev ->
        Jsoo.Firebug.console##log "ondrop triggered";
        inject Action.Incr_counter (* TODO replace with drop function *))
    in
    let ondragover = Attr.on "dragover" (fun ev ->
        Jsoo.Firebug.console##log "ondragover triggered";
        Jsoo.Dom.preventDefault ev;
        Event.Ignore
      )
    in
    let class_ = Attr.class_ "position"  in
    let attrs = ondragover :: ondrop :: style :: class_ :: attrs in
    Node.div attrs content
  in
  let occupant = [Node.p [Attr.create "draggable" "true"] [Node.text "Drag Me"]]
  in
  let%map from_box =
    (* TODO Focus change on specific dom element  *)
    let%map _ = m in
    position ~content:occupant ()
  and to_box =
    let%map _ = m in
    position ()
  (* ---- *)
  and input =
    let%map input_text = m >>| Model.input_text in
    Node.input
      [ Attr.type_ "text"
      (* The value property controls the current value of the text input, whereas the
         value attribute only controls its initial value. *)
      ; Attr.string_property "value" input_text
      (* We must update our model with the user's input to keep the virtual dom consistent
         with the actual dom. *)
      ; Attr.on_input (fun _ev text -> inject (Action.Update_input text))
      ]
      []
  and submission =
    let%map submitted_text = m >>| Model.submitted_text in
    let text =
      match submitted_text with
      | None -> "No submissions yet"
      | Some text -> "Your latest submission was: " ^ text
    in
    Node.div [] [ Node.text text ]
  in
  Node.body [] [ submission; input; submit_button; reset_button; incr_button; from_box; to_box]

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model in
    apply_action model
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action model view