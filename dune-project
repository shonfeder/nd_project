(lang dune 1.11)
(generate_opam_files true)

(name nd_project)
(version 0.1.0)

(authors "Shon Feder")
; (license MIT)
; (source (github shonfeder/alg_structs))
(maintainers "shon.feder@gmail.com")
; (homepage "https://github.com/shonfeder/alg_structs")
; (bug_reports "https://github.com/shonfeder/alg_structs/issues")
; (documentation "https://shonfeder.github.io/alg_structs/")

(package
 (name nd_project)
 (synopsis "TODO")
 (description "TODO")
 (depends
  (dune (>= 1.11.3))

  (ocaml (>= 4.08.0))
  (core_kernel (>= v0.12.2))

  (js_of_ocaml (>= 3.4.0))
  (js_of_ocaml-ppx (>= 3.4.0))
  (js_of_ocaml-ppx_deriving_json (>= 3.4.0))
  (js_of_ocaml-lwt (>= 3.4.0))
  (incr_dom (>= v0.12.0))

  (opium (>= 0.17.1))

  (ppx_deriving (>= 4.4))
  (ppx_jane (>= v0.12.0))

  (qcheck (>= 0.12))
  (qcheck-alcotest (>= 0.12))
))
