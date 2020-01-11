open! Core_kernel
open Notation

module type S = sig
  module Rule : sig
    type t
    val make : op:Symbol.logic -> mode:Rule.mode -> ?dir:Rule.dir -> unit -> t

    type mode
    [@@deriving sexp, compare, show]

    type dir
    [@@deriving sexp, compare, show]
  end

  module Formula : sig
    type t
    val equal : t -> t -> bool
    val complete : Formula.t -> t
    val to_complete : t -> Formula.t option
  end

  module Figure : sig
    type t = (Formula.t, Rule.t) Figure.t
  end
end
