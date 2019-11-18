open Core_kernel
open Incr_dom
open Natural_deduction

module Jsoo = Js_of_ocaml

module Model = struct
  type t =
    { counter : int
    ; input_text : string
    ; submitted_text : string option
    ; derivation : Notation.Figure.t
    }
  [@@deriving sexp, fields, compare]

  (* TODO *)
  let test_derivation =
    let open Notation in
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

  let set_default_input counter submitted_text =
    { counter
    ; input_text = sprintf "Default #%d" counter
    ; submitted_text
    ; derivation = test_derivation
    }

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

(** Derivation nodes *)
module DNode = struct
  open Vdom
  open Notation
  open Expression

  let elementary : Formula.elementary -> Node.t =
    let classes = ["prop"; "elementary"; "subformula"] in
    function
    | Definite F -> Node.span [Attr.classes ("falsum" :: classes)] [Node.text "⊥"]
    | Definite T -> Node.span [Attr.classes ("varum" :: classes)] [Node.text "⊤"]
    | Prop (v, args) ->
      let prop_str = S.Var.prop_to_string v in
      let args_str = args |> List.map ~f:S.Var.obj_to_string |> String.concat ~sep:"" in
      Node.span [Attr.classes classes] [Node.text (prop_str ^ args_str)]

  let operator : name:string -> sign:string -> Node.t = fun ~name ~sign ->
    Node.span [Attr.classes ["operator"; name ^ "-op"]] [Node.text sign]

  let neg_op  = operator ~name:"negation" ~sign:"¬"
  let conj_op = operator ~name:"conjunction" ~sign:"∧"
  let disj_op = operator ~name:"disjunction" ~sign:"∨"
  let imp_op  = operator ~name:"implication" ~sign:"→"

  let formula : Formula.t -> Node.t = fun f ->
    let rec subformula f = match (f : Formula.t) with
      | Elem e -> elementary e
      | Comp c -> compound c

    and compound : Formula.compound -> Node.t =
      fun c ->
        let classes = ["subformula"; "compound"] in
        let class_, content = match c with
          | Not a      -> "negation"   , [neg_op; subformula a]
          | And (a, b) -> "conjunction", [subformula a; conj_op; subformula b]
          | Or  (a, b) -> "disjunction", [subformula a; disj_op; subformula b]
          | Imp (a, b) -> "implication", [subformula a; imp_op ; subformula b]
        in
        Node.span [Attr.classes (class_ :: classes)] content
    in
    Node.span [Attr.classes ["formula"]] [subformula f]

  exception Invalid_rule

  let view_rule : Figure.Rule.t -> Node.t =
    let view_op : Symbol.logic -> Node.t = function
      | And     -> conj_op
      | Or      -> disj_op
      | Imp     -> imp_op
      | Not     -> neg_op
      | Explode -> Node.text "absurd"
      | _       -> raise Invalid_rule
    in
    let view_mode : Figure.Rule.mode -> Node.t = function
      | Intro -> Node.span [Attr.class_ "mode"] [Node.text "I"]
      | Elim  -> Node.span [Attr.class_ "mode"] [Node.text "E"]
    in
    let view_dir : Figure.Rule.dir option -> Node.t = function
      | None       -> Node.span [] []
      | Some Left  -> Node.span [Attr.class_ "direction"] [Node.text "L"]
      | Some Right -> Node.span [Attr.class_ "direction"] [Node.text "R"]
    in
    fun Figure.Rule.{op; mode; dir} ->
      Node.div [Attr.class_ "rule"] [view_op op; view_mode mode; view_dir dir]

  (* TODO This is incorrectly wrapping assumptions as proofs *)
  let derivation : Figure.t -> Node.t = fun d ->
    let assumption f =
      Node.div [Attr.class_ "assumption"] [formula f]
    in
    let rec deriv d = match (d : Figure.t) with
      | Initial ass -> assumption ass
      | Deriv {upper; lower; rule} ->
        let upper = List.map ~f:deriv upper in
        Node.div [Attr.class_ "derivation"]
          [ Node.div [Attr.class_ "inference"]
              [ Node.div [Attr.class_ "upper"] upper
              ; Node.div [Attr.class_ "lower"] [formula lower]]
          ; view_rule rule
          ]
    in
    Node.div [Attr.class_ "proof"] [deriv d]
end

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
  and derivation =
    let%map deriv = m >>| Model.derivation in
    DNode.derivation deriv
  in
  Node.div [] [ submission; input; submit_button; reset_button; incr_button
              ; from_box; to_box
              ; derivation
              ]

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model in
    apply_action model
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action model view
