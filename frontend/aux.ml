open Core_kernel
open Natural_deduction

module Jsoo = Js_of_ocaml

let log_str : string -> unit = fun str ->
  Jsoo.Firebug.console##log (Jsoo.Js.string str)

let log_err str = "[error]: " ^ str |> log_str

let log_proof : Proof.Focused.t -> unit = fun focused ->
  focused
  |> Proof.Focused.sexp_of_t
  |> Sexp.to_string_hum
  |> log_str
