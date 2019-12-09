open! Core_kernel

open Natural_deduction

type t =
  | Grow_proof of Proof.Focused.t
  | Apply_tactic of Proof.Tactic.t
[@@deriving sexp]

let should_log _ = true
