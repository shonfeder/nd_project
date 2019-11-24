open Core_kernel

(** Partial proofs and proof search *)

module Complete = struct
  open Notation
  open Notation.Expression

  type t = (Formula.t, Figure.Rule.t) Figure.t
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
      | Complete of Figure.Rule.t (* A complete rule *)
      | Promised of Figure.Rule.t (* A None dir is a place holder *)
      | Hole (* ⋮ *)
    [@@deriving sexp, compare]

    exception Promised_rule_with_dir

    let complete r = Complete r
    let promised r = match Figure.Rule.(r.dir) with
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
