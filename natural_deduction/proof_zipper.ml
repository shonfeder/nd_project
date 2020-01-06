open! Core_kernel

open Notation

module Partial = Proof_partial

(** NOTE Adapted from
    https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf *)

(* (\* TODO Replace with optional value *\)
 * type ('formula, 'rule) path =
 *   | End
 *   | Way of ('formula, 'rule) way
 * [@@deriving compare, sexp] *)

type ('formula, 'rule) path =
  { left: ('formula, 'rule) Figure.t list
  (** The derivations to left of the focus on the same line *)
  ; right: ('formula, 'rule) Figure.t list
  (** The derivations to right of the focus on the same line *)
  ; lower: 'formula
  (** The lower formula in the derivation *)
  ; rule: 'rule
  (** The rule that gets us to the lower formula *)
  ; path: ('formula, 'rule) path option
  (** The way we got to the current location *)
  }
[@@deriving compare, fields, sexp]

type ('formula, 'rule) t =
  { focus: ('formula, 'rule) Figure.t
  ; path: ('formula, 'rule) path option
  }
[@@deriving compare, fields, sexp]

type partial = (Partial.Formula.t, Partial.Rule.t) t
[@@deriving compare, sexp]
type complete = (Formula.t, Rule.t) t
[@@deriving compare, sexp]

let make ?(path=None) focus  =
  Fields.create ~focus ~path

(** Move the focus to the left sibling figure

    A B |C| D                     A |B| C D
    ---------   ==(move left)==>  ---------
       E                             E

    [move_left t] is [None] if leftmost is already in focus.
*)
let move_left {focus = x; path} =
  match path with
  | None -> None | Some {left = []; _} -> None
  | Some ({left = focus :: left; _} as way) ->
    let path = Some { way with left
                             ; right = x :: way.right
                    }
    in
    Some {focus; path}

(** Move the focus to the right sibling figure

    A |B| C D                     A B |C| D
    ---------   ==(move right)==>  ---------
       E                             E

    [move_Right t] is [None] if rightmost is already in focus. *)
let move_right {focus = x; path} =
  match path with
  | None -> None | Some {right = []; _} -> None
  | Some ({right = focus :: right; _} as way) ->
    let path = Some { way with left = x :: way.left
                             ; right
                    }
    in
    Some {focus; path}

(** Move the focus to the leftmost figure in the [upper] figures of the
    current derivation.

    |A B C D|                  |A| B C D
    |-------|  ==(move up)==>  ---------
    |   E   |                      E

    [move_up t] is [None] if the focus of [t] is an [Initial] figure or a
    [Deriv] with an empty [upper]. *)
let move_up {focus; path} = match focus with
  | Initial _ | Deriv {upper = []; _} -> None
  | Deriv {upper = (focus :: right); rule; lower} ->
    let path = Some {right; left = []; path; rule; lower}
    in
    Some {focus; path}

(* let down' : (('formula, 'rule) t * 'rule) option = Some ((make lower, rule)) in
 * (\* Some (make focus ~right ~down:down') *\)
 * {left = []
 * ; right = []
 * ; focus = focus
 * ; down = Some {focus}} *)

(** Move the focus to the derivation that follows from the current focus.

    [move_down] is [None] if the concluding inference figure is already in
    focus .*)
let move_down t = match t.path with
  | None -> None
  | Some {left; right; path; rule; lower} ->
    let focus =
      Figure.deriv (List.rev left @ [t.focus] @ right)
        ~rule
        lower
        (* down *)
    in
    Some {focus; path}

let rec move_bottom t = match move_down t with
  | None   -> t
  | Some t -> move_bottom t

let rule t = match t.path with
  | None   -> None
  | Some p -> Some p.rule

let lower t = match t.path with
  | None   -> None
  | Some p -> Some p.lower

let focus t = t.focus
let peek_left t  = List.hd t.left
let peek_right t = List.hd t.right
let peek_down t  = move_down t |> Option.map ~f:focus

let peek_upper t = match t.focus with
  | Initial _ -> None
  | Deriv d   -> Some (Figure.upper d)

let peek_up t = Option.(peek_upper t >>= List.hd)

let of_figure : ('formula, 'rule) Figure.t -> ('formula, 'rule) t =
  fun fig -> make fig

let to_figure : ('formula, 'rule) t -> ('formula, 'rule) Figure.t =
  fun t -> move_bottom t |> focus

let map : ('formula, 'rule) t -> f:(('formula, 'rule) Figure.t -> ('formula', 'rule') Figure.t) -> ('formula', 'rule') t =
  fun t ~f -> {t with focus = f t.focus}

(** [insert_focus z f] is [z] with the focused figure replaced with [f] *)
let insert_focus = fun z x -> map ~f:(Fn.const x) z

(** [insert_left fig z] inserts is [Some new_proof] with [fig] inserted left of
    the focus, unless the final formula is focused, in which case it is [None]. *)
let insert_left fig t = match t.path with
  | None -> None
  | Some ({left; _} as w) ->
    Some {t with path = Some {w with left = fig :: left}}
