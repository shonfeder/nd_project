open! Core_kernel

open Notation

module Complete_rule = Rule

module Rule = struct
  type t =
    | Complete of Complete_rule.t (* A complete rule *)
    | Promised of Complete_rule.t (* A None dir is a place holder *)
    | Hole (* ⋮ *)
  [@@deriving sexp, compare]

  exception Promised_rule_with_dir

  let complete r = Complete r
  let promised r = match Complete_rule.(r.dir) with
    | None   -> Promised r
    | Some _ -> raise Promised_rule_with_dir
  let hole      = Hole
end

module Formula = struct
  type t =
    | Complete of Formula.t
    | Promised of string (* id for formulas which must the same *)
    | Hole (* ⋮ *)
  [@@deriving sexp, compare]

  let complete f = Complete f
  let promised str = Promised str
  let hole = Hole

  let get_complete = function
    | Complete f -> Some f
    | _ -> None
end

module Figure = struct
  type t = (Formula.t, Rule.t) Figure.t
  [@@deriving sexp, compare]

  type deriv = (Formula.t, Rule.t) Figure.deriv
  [@@deriving sexp, compare]
end
