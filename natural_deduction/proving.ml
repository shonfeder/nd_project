open Core_kernel

(** Partial proofs and proof search *)

module Partial = struct
  open Notation
  open Notation.Expression

  (* TODO Seal constructors *)
  module Lower = struct
    type t =
      | Formula of Formula.t
      | Holder  of string (* id for formulas which must the same *)
      | Hole (* ⋮ *)
    [@@deriving sexp, compare]

    let formula f  = Formula f
    let holder str = Holder str
    let hole       = Hole
  end

  (* TODO Seal constructors *)
  module Rule = struct
    type t =
      | Rule    of Figure.Rule.t (* A complete rule *)
      | Partial of Figure.Rule.t (* A None dir is a place holder *)
      | Hole (* ⋮ *)
    [@@deriving sexp, compare]

    exception Partial_rule_with_dir

    let rule r    = Rule r
    let partial r = match Figure.Rule.(r.dir) with
      | None   -> Partial r
      | Some _ -> raise Partial_rule_with_dir
    let hole      = Hole
  end

  type t =
    | Partial  of deriv
    | Complete of Figure.t
  [@@deriving sexp, fields, compare]
  and deriv =
    { upper : t list option
    ; lower : Lower.t
    ; rule  : Rule.t
    }
  [@@deriving sexp, compare]
end
