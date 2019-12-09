open! Core_kernel

type t =
  | Apply_rule of Rule.t
  | Reiter
[@@deriving sexp, compare, show { with_path = false }]

let to_string = function
  | Apply_rule r -> "Apply rule " ^ (Rule.to_string r)
  | t -> show t


type 'a err =
  [> `Initial
  |  `Not_a_hole
  ] as 'a
[@@deriving show]

type 'err result = (Proof_zipper.partial, 'err err) Result.t
type 'err application = Proof_zipper.partial -> 'err result
