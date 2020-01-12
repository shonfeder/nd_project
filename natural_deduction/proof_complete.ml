open Notation

module Rule = Rule

module Formula = struct
  include Formula
  let complete x = x
  let to_complete x = Some x

  module Infix_exn = Infix
end

module Figure = struct
  include Figure
  type t = (Formula.t, Rule.t) Figure.t
  [@@deriving sexp, compare]

  let equal = Figure.equal ~formula:Formula.compare ~rule:Rule.compare
  let to_string : t -> string = Figure.to_string ~formula:Formula.to_string ~rule:Rule.to_string

  let to_complete : t -> t option = fun t -> Some t
  let complete t = t
end

(* type t = (Formula.t, Rule.t) Figure.t
 * [@@deriving sexp, compare] *)

