open! Core_kernel

(** Partial proofs and proof search *)
module Complete = struct
  module Rule = Rule
  open Notation

  type t = (Formula.t, Rule.t) Figure.t
  [@@deriving sexp, compare]

  let to_string : t -> string = Figure.to_string ~formula:Formula.to_string ~rule:Rule.to_string
end

module Partial = Proof_partial
module Zipper  = Proof_zipper
module Tactic  = Proof_interaction.Tactic
module Focused = Proof_interaction.Focused
