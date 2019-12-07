open Core_kernel

module Partial = Proof_partial
module Zipper = Proof_zipper

(** The tactics that can be used to develop a proof *)
module Tactic_t = struct
  type t =
    | Intro_imp
    | Elim_and
  [@@deriving sexp, compare, show { with_path = false }]

  let to_string = show
end

module Tactic_aliases = struct
  type 'a err =
    [> `Initial
    |  `Not_a_hole
    ] as 'a

  type 'err result = (Zipper.partial, 'err err) Result.t
  type 'err application = Zipper.partial -> 'err result
end

module Tactic : sig
  include module type of Tactic_t
  include module type of Tactic_aliases

  val intro_imp: _ application
  val elim_and : _ application

  val apply : Zipper.partial -> t -> _ result
end = struct
  include Tactic_t
  include Tactic_aliases

  open Notation

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

  let elim_and _z = raise (Failure "TODO: elim_and")



  let apply z tac : 'err result = match (tac : t) with
    | Intro_imp -> intro_imp z
    | Elim_and  -> elim_and z
    (* TODO Insert antecedent ... consequent partial into upper *)
end

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
