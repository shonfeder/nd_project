open Notation

module Rule = Rule

module Formula = struct
  include Formula
  let complete x = x
  let to_complete x = Some x
end

module Figure = struct
  include Figure
  type t = (Formula.t, Rule.t) Figure.t
  [@@deriving sexp, compare]

  let to_string : t -> string = Figure.to_string ~formula:Formula.to_string ~rule:Rule.to_string
end

(* type t = (Formula.t, Rule.t) Figure.t
 * [@@deriving sexp, compare] *)

