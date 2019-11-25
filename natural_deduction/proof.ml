open Core_kernel

(** Partial proofs and proof search *)

module Complete = struct
  open Notation

  module Rule = struct
    type mode =
      | Intro
      | Elim
    [@@deriving sexp, compare, show]

    type dir =
      | Left
      | Right
    [@@deriving sexp, compare, show]

    type t =
      { op: Symbol.logic
      ; mode: mode
      ; dir: dir option
      }
    [@@deriving sexp, compare, show, fields]

    let equal a b = (compare a b = 0)
    let to_string = show

    let make ~op ~mode ?dir () = Fields.create ~op ~mode ~dir
  end

  type t = (Formula.t, Rule.t) Figure.t
  [@@deriving sexp, compare]
end

module Partial = struct
  open Notation


  module Formula = struct
    type t =
      | Complete of Formula.t
      | Promised of string (* id for formulas which must the same *)
      | Hole (* ⋮ *)
    [@@deriving sexp, compare]

    let complete f   = Complete f
    let promised str = Promised str
    let hole         = Hole
  end

  module Rule = struct
    type t =
      | Complete of Complete.Rule.t (* A complete rule *)
      | Promised of Complete.Rule.t (* A None dir is a place holder *)
      | Hole (* ⋮ *)
    [@@deriving sexp, compare]

    exception Promised_rule_with_dir

    let complete r = Complete r
    let promised r = match Complete.Rule.(r.dir) with
      | None   -> Promised r
      | Some _ -> raise Promised_rule_with_dir
    let hole      = Hole
  end

  type t = (Formula.t, Rule.t) Figure.t
  [@@deriving sexp, compare]

  type deriv = (Formula.t, Rule.t) Figure.deriv
  [@@deriving sexp, compare]
end

module Zipper = struct
  open Notation

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
  [@@deriving compare, fields]

  let make ?(left=[]) ?(right=[]) ?(down=[]) focus =
    Fields.create ~focus ~left ~right ~down

  let of_figure : ('formula, 'rule) Figure.t -> ('formula, 'rule) t =
    fun fig -> make fig

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
    | (x :: xs) ->
      Some {t with left  = focus :: left
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

  let peek_left t  = List.hd t.left
  let peek_right t = List.hd t.right
  let peek_down t  = List.hd t.down

  let peek_upper t = match t.focus with
    | Initial _ -> None
    | Deriv d   -> Some (Figure.upper d)

  let peek_up t = Option.(peek_upper t >>= List.hd)
end
