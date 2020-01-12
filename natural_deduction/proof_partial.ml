open! Core_kernel

open Notation

module Complete_rule = Rule

module Rule = struct
  type t =
    | Complete of Complete_rule.t (* A complete rule *)
    | Promised of Complete_rule.t (* A None dir is a place holder *)
    | Hole (* ⋮ *)
  [@@deriving sexp, compare, eq]

  type mode = Rule.mode
  [@@deriving sexp, compare, show]

  type dir = Rule.dir
  [@@deriving sexp, compare, show]

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

  let to_complete = function
    | Complete r -> Some r
    | _ -> None
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

  let to_complete_exn f = to_complete f |> Option.value_exn

  let prop s = Formula.prop s |> complete

  (* Only for testing *)
  module Infix_exn = struct
    module I = Formula.Infix
    let app_op o a b =
      let a, b = to_complete_exn a, to_complete_exn b in
      complete (o a b)

    let (&&) = app_op I.(&&)
    let (||) = app_op I.(||)
    let (=>) = app_op I.(=>)
    let (!!) a = to_complete_exn a |> I.(!!) |> complete
  end
end

module Figure = struct
  type t = (Formula.t, Rule.t) Figure.t
  [@@deriving sexp, compare]

  let equal = Figure.equal ~formula:Formula.compare ~rule:Rule.compare

  type deriv = (Formula.t, Rule.t) Figure.deriv
  [@@deriving sexp, compare]

  let to_string = Figure.to_string ~formula:Formula.to_string ~rule:Rule.to_string

  let rec to_complete : t -> Proof_complete.Figure.t option = function
    | Initial f -> Formula.to_complete f |> Option.map ~f:(fun x -> Figure.Initial x)
    | Deriv d ->
      let open Option.Let_syntax in
      let%bind upper = List.map ~f:to_complete d.upper |> Option.all in
      let%bind lower = Formula.to_complete d.lower in
      let%map rule  = Rule.to_complete d.rule in
      Figure.Deriv {upper; lower; rule}

  let complete t = Figure.map ~formula:Formula.complete ~rule:Rule.complete t
end
