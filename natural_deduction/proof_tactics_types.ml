open! Core_kernel

type t =
  | Apply_rule of Rule.t
  | Reiter
[@@deriving sexp, compare, show { with_path = false }]

let to_string = function
  | Apply_rule r -> "Apply rule " ^ (Rule.to_string r)
  | t -> show t


type err =
  [ `Initial
  | `Not_a_hole
  | `Iter_on_noninitial
  | `Unknown of string
  | `No_tactics
  ]
[@@deriving show, sexp]


type result = (Proof_zipper.partial, err) Result.t
type application = Proof_zipper.partial -> result
