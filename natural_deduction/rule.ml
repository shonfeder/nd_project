open! Core_kernel
open Notation

type mode =
  | Intro
  | Elim
[@@deriving sexp, compare, show]

let mode_to_string = function
  | Intro -> "I"
  | Elim  -> "E"

type dir =
  | Left
  | Right
[@@deriving sexp, compare, show]

let dir_to_string = function
  | Left  -> "L"
  | Right -> "R"

type t =
  { op: Symbol.logic
  ; mode: mode
  ; dir: dir option
  }
[@@deriving sexp, compare, show, fields]

let equal a b = (compare a b = 0)
let to_string {op; mode; dir} =
  let op = Symbol.logic_to_string op in
  let mode = mode_to_string mode in
  let dir = match dir with
    | None   -> ""
    | Some d -> "-" ^ dir_to_string d in
  Printf.sprintf "%s%s%s" op mode dir

let make ~op ~mode ?dir () = Fields.create ~op ~mode ~dir
