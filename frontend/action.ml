open Core_kernel

type t =
  | Reset_counter
  | Incr_counter
  | Update_input of string
  | Submit_input
  | Grow_proof of Natural_deduction.Proof.Focused.t
[@@deriving sexp]

let should_log _ = true
