open Core_kernel
open Incr_dom
open Natural_deduction

module Make (I : sig val inject: Action.t -> Vdom.Event.t end) = struct

  (* Node templates used in creating views. These should not know about any code
     in [Naural_deduction] *)
  module DerivNode = struct
    (* TODO refactor out all magic string classes into this module *)
    open Vdom

    let _div class_ classes content =
      let classes = class_ :: classes in
      Node.div [Attr.classes classes] content

    let lower_div ?(classes=[]) content =
      _div "lower" classes content

    let upper_div ?(classes=[]) content =
      _div "upper" classes content

    let rule_div ?(classes=[]) content =
      _div "rule" classes content

    let inference_div ?(classes=[]) content =
      _div "inference" classes content

    let derivation_div ?(classes=[]) content =
      _div "derivation" classes content

    let proof_div ?(classes=[]) content =
      _div "proof" classes content

    let assumption_div ?(classes=[]) content =
      _div "assumption" classes content

  end

  module Derivation = struct
    (** Derivation nodes *)
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
      let attrs =
        [ Attr.classes ["formula"]
        ; Attr.on_click (fun _ev -> I.inject (Action.Sprout_twig "blah"))
        ]
      in
      Node.span attrs [subformula f]

    exception Invalid_rule

    let rule_content : Figure.Rule.t -> Node.t list =
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
      fun Figure.Rule.{op; mode; dir} -> [view_op op; view_mode mode; view_dir dir]

    let view_assumption : Formula.t -> Node.t = fun ass ->
      DerivNode.assumption_div [formula ass]

    (* TODO This is incorrectly wrapping assumptions as proofs *)
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

    let view_initial : Partial.Formula.t -> Node.t = function
      | Complete f   -> Derivation.view_assumption f
      | Promised str -> DerivNode.assumption_div ~classes:["promised"] [Node.text str]
      | Hole         -> DerivNode.assumption_div ~classes:["hole"] [proof_hole_span]

    let view_formula : Partial.Formula.t -> Node.t = function
      | Complete f   -> DerivNode.lower_div [Derivation.formula f]
      | Promised str -> DerivNode.lower_div ~classes:["promissed"] [Node.text str]
      | Hole         -> DerivNode.lower_div ~classes:["hole"] [proof_hole_span]

    let view_rule : Partial.Rule.t -> Node.t = function
      | Complete r -> DerivNode.rule_div (Derivation.rule_content r)
      | Promised r -> DerivNode.rule_div ~classes:["promised"] (Derivation.rule_content r)
      | Hole       -> DerivNode.rule_div ~classes:["hole"] [rule_hole_span]

    let view : Partial.t -> Node.t = fun t ->
      let rec figure : Partial.t -> Node.t = function
        | Initial f -> view_initial f
        | Deriv d -> deriv d
      and deriv : Partial.deriv -> Node.t =
        fun {upper; lower; rule} ->
          DerivNode.derivation_div ~classes:["partial"]
            [ DerivNode.inference_div ~classes:["partial"]
                [ view_upper upper
                ; view_formula lower ]
            ; view_rule rule ]
      and view_upper : Partial.t list -> Node.t = function
        | []       -> DerivNode.upper_div ~classes:["hole"] [proof_hole_span]
        | upper -> DerivNode.upper_div (List.map ~f:figure upper)
      in
      DerivNode.proof_div ~classes:["partial"] [figure t]
  end
end
