open! Core_kernel

open Notation

module Partial = Proof_partial

type ('formula, 'rule) t =
  { left: ('formula, 'rule) Figure.t list
  (** The derivations to left of the focus on the same line *)
  ; focus: ('formula, 'rule) Figure.t
  (** The derivation in focus *)
  ; right: ('formula, 'rule) Figure.t list
  (** The derivations to right of the focus on the same line *)
  ; down: ('formula, 'rule) t list
  (** The derivations derivable from those on the current line *)
  }
[@@deriving compare, fields, sexp]

type partial = (Partial.Formula.t, Partial.Rule.t) t
[@@deriving compare, sexp]
type complete = (Formula.t, Rule.t) t
[@@deriving compare, sexp]

let make ?(left=[]) ?(right=[]) ?(down=[]) focus =
  Fields.create ~focus ~left ~right ~down

(** Move the focus to the left sibling figure

    A B |C| D                     A |B| C D
    ---------   ==(move left)==>  ---------
       E                             E

    [move_left t] is [None] if leftmost is already in focus.
*)
let move_left ({left; focus; right; _} as t) =
  match left with
  | [] -> None
  | (x :: xs) ->
    Some {t with left  = xs
               ; focus = x
               ; right = focus :: right
         }

(** Move the focus to the right sibling figure

    A |B| C D                     A B |C| D
    ---------   ==(move right)==>  ---------
       E                             E

    [move_Right t] is [None] if rightmost is already in focus. *)
let move_right ({left; focus; right; _} as t) =
  match right with
  | [] -> None
  | (x :: xs) -> Some {t with left  = focus :: left
                            ; focus = x
                            ; right = xs
                      }

(** Move the focus to the leftmost figure in the [upper] figures of the
    current derivation.

    |A B C D|                  |A| B C D
    |-------|  ==(move up)==>  ---------
    |   E   |                      E

    [move_up t] is [None] if the focus of [t] is an [Initial] figure or a
    [Deriv] with an empty [upper]. *)
let move_up t = match t.focus with
  | Initial _ -> None
  | Deriv {upper = []; _} -> None
  | Deriv {upper = (focus :: right); _} ->
    Some (make focus ~right ~down:(t :: t.down))

(** Move the focus to the derivation that follows from the current focus.

    [move_down] is [None] if the concluding inference figure is already in
    focus .*)
let move_down t = match t.down with
  | []       -> None
  | (t :: _) -> Some t

let of_figure : ('formula, 'rule) Figure.t -> ('formula, 'rule) t =
  fun fig -> make fig

let rec to_figure : ('formula, 'rule) t -> ('formula, 'rule) Figure.t =
  fun t -> match move_down t with
    | None -> t.focus
    | Some t -> to_figure t

let focus t = t.focus
let peek_left t  = List.hd t.left
let peek_right t = List.hd t.right
let peek_down t  = List.hd t.down

let peek_upper t = match t.focus with
  | Initial _ -> None
  | Deriv d   -> Some (Figure.upper d)

let peek_up t = Option.(peek_upper t >>= List.hd)

let map : ('formula, 'rule) t -> f:(('formula, 'rule) Figure.t -> ('formula', 'rule') Figure.t) -> ('formula', 'rule') t =
  fun t ~f -> {t with focus = f t.focus}

let insert_focus = fun z x -> map ~f:(Fn.const x) z

let insert_left x t = {t with left = x :: t.left}
let insert_up x t =
  Option.(move_up t >>= fun up -> insert_left x up |> move_down)
