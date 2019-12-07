open! Core_kernel

type t =
  | Intro_imp
  | Elim_and
[@@deriving sexp, compare, show { with_path = false }]

let to_string = show

type 'a err =
  [> `Initial
  |  `Not_a_hole
  ] as 'a

type 'err result = (Proof_zipper.partial, 'err err) Result.t
type 'err application = Proof_zipper.partial -> 'err result
