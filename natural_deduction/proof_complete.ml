module Rule = Rule
open Notation

type t = (Formula.t, Rule.t) Figure.t
[@@deriving sexp, compare]

let to_string : t -> string = Figure.to_string ~formula:Formula.to_string ~rule:Rule.to_string
