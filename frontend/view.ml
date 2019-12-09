open Core_kernel
open Incr_dom

open Natural_deduction

module Jsoo = Js_of_ocaml

let (%) f g x = f (g x)

(* Node templates used in creating views. These should not know about any code
   in [Naural_deduction] *)
module DerivNode = struct
  (* TODO refactor out all magic string classes into this module *)
  open Vdom

  let _div class_ ?(classes=[]) ?(attrs=[]) content =
    let classes = class_ :: classes in
    Node.div (Attr.classes classes :: attrs) content

  let lower_div      = _div "lower"
  let upper_div      = _div "upper"
  let rule_div       = _div "rule"
  let inference_div  = _div "inference"
  let derivation_div = _div "derivation"
  let proof_div      = _div "proof"
  let assumption_div = _div "assumption"

  let proof_ui_div   = _div "proof-ui"

  let tactic_ul content = Node.ul [Attr.class_ "tactics-menu"] content
  let tactic_li handler content = Node.li [handler; Attr.class_ "tactics-item"] content
end


module Make (I : sig val inject: Action.t -> Vdom.Event.t end) = struct

  let sprout_on_click_attr zipper =
    Vdom.Attr.on_click (fun _ev -> I.inject (Action.Grow_proof zipper))

  module Derivation = struct
    (** Derivation nodes *)
    open Vdom
    open Notation

    (* TODO *)
    let _ = I.inject

    let elementary : Formula.elementary -> Node.t =
      let classes = ["prop"; "elementary"; "subformula"] in
      function
      | Definite F -> Node.span [Attr.classes ("falsum" :: classes)] [Node.text "⊥"]
      | Definite T -> Node.span [Attr.classes ("varum" :: classes)] [Node.text "⊤"]
      | Prop (v, args) ->
        let prop_str = Symbol.Var.prop_to_string v in
        let args_str = args |> List.map ~f:Symbol.Var.obj_to_string |> String.concat ~sep:"" in
        Node.span [Attr.classes classes] [Node.text (prop_str ^ args_str)]

    let operator : name:string -> sign:string -> Node.t = fun ~name ~sign ->
      Node.span [Attr.classes ["operator"; name ^ "-op"]] [Node.text sign]

    let neg_op  = operator ~name:"negation" ~sign:"¬"
    let conj_op = operator ~name:"conjunction" ~sign:"∧"
    let disj_op = operator ~name:"disjunction" ~sign:"∨"
    let imp_op  = operator ~name:"implication" ~sign:"→"

    let formula : ?focused:Proof.Focused.t -> Formula.t -> Node.t = fun ?focused f ->
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
      let attrs =
        let classes = [Attr.classes ["formula"]] in
        match focused with
        | None   -> classes
        | Some z -> sprout_on_click_attr z :: classes
      in
      Node.span attrs [subformula f]

    exception Invalid_rule

    module Rule = Proof.Complete.Rule

    let rule_content : Rule.t -> Node.t list =
      let view_op : Symbol.logic -> Node.t = function
        | And     -> conj_op
        | Or      -> disj_op
        | Imp     -> imp_op
        | Not     -> neg_op
        | Explode -> Node.text "absurd"
        | _       -> raise Invalid_rule
      in
      let view_mode : Rule.mode -> Node.t = function
        | Intro -> Node.span [Attr.class_ "mode"] [Node.text "I"]
        | Elim  -> Node.span [Attr.class_ "mode"] [Node.text "E"]
      in
      let view_dir : Rule.dir option -> Node.t = function
        | None       -> Node.span [] []
        | Some Left  -> Node.span [Attr.class_ "direction"] [Node.text "L"]
        | Some Right -> Node.span [Attr.class_ "direction"] [Node.text "R"]
      in
      fun Rule.{op; mode; dir} -> [view_op op; view_mode mode; view_dir dir]

    let view_assumption : ?attrs:(Attr.t list) -> Formula.t -> Node.t =
      fun ?attrs ass -> DerivNode.assumption_div ?attrs [formula ass]

    let view : Proof.Complete.t -> Node.t = fun d ->
      let rec figure : Proof.Complete.t -> Node.t =
        function | Initial ass -> view_assumption ass
                 | Deriv d     -> deriv d
      and deriv : _ Figure.deriv -> Node.t =
        fun {upper; lower; rule} ->
          DerivNode.derivation_div
            [ DerivNode.inference_div
                [ DerivNode.upper_div (List.map ~f:figure upper)
                ; DerivNode.lower_div [formula lower] ]
            ; DerivNode.rule_div (rule_content rule) ]
      in
      DerivNode.proof_div [figure d]
  end

  module Partial_derivation = struct
    open Vdom
    open Proof

    let proof_hole_span = Node.span [Attr.classes ["hole"; "proof-hole"]] [Node.text "⋮"]
    let rule_hole_span  = Node.span [Attr.classes ["hole"; "rule-hole"]] []

    let view_initial : ?focused:Focused.t -> Partial.Formula.t -> Node.t =
      fun ?focused f ->
      let attrs = match focused with
        | None -> []
        | Some z -> [sprout_on_click_attr z]
      in
      match f with
      | Complete f   -> Derivation.view_assumption ~attrs f
      | Promised str -> DerivNode.assumption_div ~attrs ~classes:["promised"] [Node.text str]
      | Hole         -> DerivNode.assumption_div ~attrs ~classes:["hole"] [proof_hole_span]

    let view_formula : ?focused:Focused.t -> Partial.Formula.t -> Node.t =
      fun ?focused f ->
      let attrs = match focused with
        | None -> []
        | Some z -> [sprout_on_click_attr z]
      in
      match f with
      | Complete f   -> DerivNode.lower_div [Derivation.formula ?focused f]
      | Promised str -> DerivNode.lower_div ~attrs ~classes:["promissed"] [Node.text str]
      | Hole         -> DerivNode.lower_div ~attrs ~classes:["hole"] [proof_hole_span]

    let view_rule : Partial.Rule.t -> Node.t = function
      | Complete r -> DerivNode.rule_div (Derivation.rule_content r)
      | Promised r -> DerivNode.rule_div ~classes:["promised"] (Derivation.rule_content r)
      | Hole       -> DerivNode.rule_div ~classes:["hole"] [rule_hole_span]

    let view_tactic : Proof.Tactic.t -> Node.t =
      fun tactic ->
      let handler =
        Vdom.Attr.on_click (fun _ev -> I.inject (Action.Apply_tactic tactic))
      in
      DerivNode.tactic_li handler [Node.text @@ Proof.Tactic.to_string tactic]

    let view_menu : Proof.Tactic.t list -> Node.t =
      fun tactics ->
      List.map ~f:view_tactic tactics
      |> DerivNode.tactic_ul

    (* let view : Proof.Focused.t -> Node.t = fun {proof; menu} ->
     *   let rec figure : Partial.Figure.t -> Node.t = function
     *     | Initial f -> view_initial f
     *     | Deriv d   -> deriv d
     *   and deriv : Partial.Figure.deriv -> Node.t =
     *     fun {upper; lower; rule} ->
     *       DerivNode.derivation_div ~classes:["partial"]
     *         [ DerivNode.inference_div ~classes:["partial"]
     *             [ view_upper upper
     *             ; view_formula lower ]
     *         ; view_rule rule ]
     *   and view_upper : Partial.Figure.t list -> Node.t = function
     *     | []    -> DerivNode.upper_div ~classes:["hole"] [proof_hole_span]
     *     | upper -> DerivNode.upper_div (List.map ~f:figure upper)
     *   in
     *   DerivNode.proof_ui_div
     *     [ DerivNode.proof_div ~classes:["partial"] [figure proof]
     *     ; view_menu menu ] *)
  end

  module Proof = struct
    open Vdom
    open Proof
    open Notation

    let view : Focused.t -> Node.t = fun ({proof; tactics} as focused) ->
      let rec view_proof : Zipper.partial -> Node.t =
        fun proof ->
          let focused = {focused with proof} in
          match Zipper.focus proof with
          | Figure.Initial i -> Partial_derivation.view_initial ~focused i
          | Figure.Deriv {rule; lower; _} ->
            DerivNode.derivation_div ~classes:["partial"]
              [ DerivNode.inference_div ~classes:["partial"]
                  [ view_upper (Zipper.move_up proof)
                  ; Partial_derivation.view_formula ~focused lower ]
              ; Partial_derivation.view_rule rule ]

      and view_upper : Zipper.partial option -> Node.t =
        fun proof ->
          let rec get_uppers z = match z with
            | None -> []
            | Some z -> z :: (get_uppers @@ Zipper.move_right z)
          in
          match get_uppers proof with
          | []     -> DerivNode.upper_div ~classes:["hole"] [Partial_derivation.proof_hole_span]
          | uppers -> DerivNode.upper_div (List.map ~f:view_proof uppers)
      in
      DerivNode.proof_ui_div
        [ DerivNode.proof_div ~classes:["partial"] [view_proof (Zipper.move_bottom proof)]
        ; Partial_derivation.view_menu tactics ]
  end
end
