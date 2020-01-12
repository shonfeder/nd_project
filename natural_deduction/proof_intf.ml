open! Core_kernel
open Notation

module type S = sig
  module Rule : sig
    type t
    [@@deriving sexp, eq]

    val make : op:Symbol.logic -> mode:Rule.mode -> ?dir:Rule.dir -> unit -> t

    type mode
    [@@deriving sexp, compare, show]

    type dir
    [@@deriving sexp, compare, show]
  end

  module Formula : sig
    type t
    [@@deriving sexp, eq]

    val complete : Formula.t -> t
    val to_complete : t -> Formula.t option
    val to_string : t -> string
    val prop : string -> t

    module Infix_exn : sig
      val ( && ) : t -> t -> t
      val ( || ) : t -> t -> t
      val ( => ) : t -> t -> t
      val ( !! ) : t -> t
    end
  end

  module Figure : sig
    type t = (Formula.t, Rule.t) Figure.t
    [@@deriving sexp, eq]
    val to_string : t -> string

    val to_complete : t -> Proof_complete.Figure.t option

    val complete : Proof_complete.Figure.t -> t
  end
end
