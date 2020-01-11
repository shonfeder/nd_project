open! Core_kernel

module Complete = Proof_complete
module Partial = Proof_partial

(** The interface shared by both [Complete] and [Partial] proofs *)
module type S = Proof_intf.S

module Zipper   = Proof_zipper
module Tactic   = Proof_interaction.Tactic
module Focused  = Proof_interaction.Focused
