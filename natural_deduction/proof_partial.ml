open! Core_kernel

open Notation

module Complete_rule = Rule

module Rule = struct
  type t =
    | Complete of Complete_rule.t (* A complete rule *)
    | Promised of Complete_rule.t (* A None dir is a place holder *)
    | Hole (* ⋮ *)
  [@@deriving sexp, compare]

  let to_string = function
    | Complete r | Promised r -> Complete_rule.to_string r
    | Hole -> "..."

  exception Promised_rule_with_dir

  let complete r = Complete r
  let promised r = match Complete_rule.(r.dir) with
    | None   -> Promised r
    | Some _ -> raise Promised_rule_with_dir
  let hole      = Hole

  let make ~op ~mode ?dir () = Complete_rule.make ~op ~mode ?dir () |> complete
end

module Formula = struct
  type t =
    | Complete of Formula.t
    | Promised of string (* id for formulas which must be the same *)
    | Hole (* ⋮ *)
  [@@deriving sexp, compare, eq]

  let to_string = function
    | Complete f -> Formula.to_string f
    | Promised p -> p
    | Hole       -> "..."

  let complete f = Complete f
  let promised str = Promised str
  let hole = Hole

  let is_hole = function
    | Hole -> true
    | _    -> false

  let to_complete = function
    | Complete f -> Some f
    | _ -> None
end

module Figure = struct
  type t = (Formula.t, Rule.t) Figure.t
  [@@deriving sexp, compare]

  type deriv = (Formula.t, Rule.t) Figure.deriv
  [@@deriving sexp, compare]

  let to_string = Figure.to_string ~formula:Formula.to_string ~rule:Rule.to_string
end
