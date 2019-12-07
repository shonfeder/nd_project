open! Core_kernel

type t =
  | Grow_proof of Natural_deduction.Proof.Focused.t
[@@deriving sexp]

let should_log _ = true
