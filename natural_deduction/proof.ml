open Core_kernel

(** Partial proofs and proof search *)

module Complete = struct
  open Notation
  open Notation.Expression

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

    let to_string = show

    let make ~op ~mode ?dir () = Fields.create ~op ~mode ~dir
  end

  type t = (Formula.t, Rule.t) Figure.t
  [@@deriving sexp, compare]
end

module Partial = struct
  open Notation
  open Notation.Expression

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

  (* type zipper =
   *   { up: t option
   *   ; focus: deriv
   *   ; down: t option
   *   } *)

  (* let zipper  *)
end
