open Core_kernel

type t =
  | Reset_counter
  | Incr_counter
  | Update_input of string
  | Submit_input
  (* TODO *)
  | Sprout_twig of string
[@@deriving sexp]

let should_log _ = true
