open Core_kernel

(* TODO Seal modules with interfaces  *)

let motto = Excerpt.gentzen_nd
    {|To the concepts 'object', 'function, 'predicate', 'proposition', 'theorem',
'axiom', 'proof' 'inference', etc., in logic and mathematics there correspond,
in the formalization of these disciplines, certain symbols or combinations of
symbols.|}
    (`Pg 69)

let ie description page = Excerpt.gentzen_nd description (`Pg page)

module Symbol = struct
  type prop = T | F
  [@@deriving sexp, eq]

  let prop_to_string = function
    | T -> "T"
    | F -> "F"

  type func = Plus | Minus | Times

  type pred = EQ | LT

  type logic =
    | And
    | Or
    | Imp
    | Equiv
    | Not
    | All
    | Exists

  type const =
    | Object of int
    | Function of func
    | Prop of prop
    | Pred of pred
    | Logic of logic

  module Var : sig
    type prop = private string
    [@@deriving sexp, eq]
    type obj = private string
    [@@deriving sexp, eq]

    type t = private
      | Prop of prop
      | Obj of obj

    val p : string -> prop
    val o : string -> obj
    val v : string -> t

    val sexp_of_prop : prop -> Sexp.t
    val sexp_of_obj  : obj -> Sexp.t
    val prop_of_sexp : Sexp.t -> prop
    val obj_of_sexp  : Sexp.t -> obj

    val prop_to_string : prop -> string
    val obj_to_string : obj -> string
    val to_string : t -> string
  end = struct
    type prop = string
    [@@deriving sexp, eq]
    type obj = string
    [@@deriving sexp, eq]

    let prop_to_string : prop -> string = Fun.id
    let obj_to_string : prop -> string = Fun.id

    type t =
      | Prop of prop
      | Obj of obj
    [@@deriving sexp]

    let to_string : t -> string = function
      | Prop s -> s
      | Obj s -> s

    let p v : prop = v
    let o v : obj = v

    let v v =
      if String.get v 0 |> Char.is_uppercase then
        Prop (p v)
      else
        Obj (o v)
  end

  type t =
    | Const of const
    | Var of Var.t
end

module Expression = struct

  module S = Symbol

  module Formula = struct

    type elementary =
      | Definite of S.prop
      | Prop of S.Var.prop * S.Var.obj list
    [@@deriving sexp, eq]

    let elementary_to_string = function
      | Definite v -> S.prop_to_string v
      | Prop (v, args) ->
        (S.Var.prop_to_string v) ^
        (args
         |> List.map ~f:S.Var.obj_to_string
         |> String.concat ~sep:"")

    (** "The concept of a propositional expression, called a 'formula' for short "*)
    type t =
      | Elem of elementary
      | Comp of compound
    [@@deriving sexp, eq]
    and compound =
      | Not of t
      | And of t * t
      | Or  of t * t
      | Imp of t * t
      (* TODO quantifiers *)
    [@@deriving sexp, eq]

    (* (\* TODO Does this really gain from GADTs? *\)
     * (\** "The concept of a propositional expression, called a 'formula' for short "*\)
     * type t =
     *   | Elem of elementary
     *   | Comp of compound
     * [@@deriving sexp, eq]
     * and compound =
     *   | Not of t
     *   | And of t * t
     *   | Or  of t * t
     *   | Imp of t * t
     *   (\* TODO quantifiers *\)
     * [@@deriving sexp, eq] *)

    let rec to_string = function
      | Elem e -> elementary_to_string e
      | Comp c -> compound_to_string c
    and compound_to_string : compound -> string = function
      | Not t      -> "-" ^ to_string t
      | And (a, b) -> Printf.sprintf "(%s && %s)" (to_string a) (to_string b)
      | Or  (a, b) -> Printf.sprintf "(%s || %s)" (to_string a) (to_string b)
      | Imp (a, b) -> Printf.sprintf "(%s => %s)" (to_string a) (to_string b)

    let ie = ie "finite sequences of symbols" 70

    let prop v   = Elem (Prop (S.Var.p v, []))
    let def s    = Elem (Definite s)
    let not_ t   = Comp (Not t)
    let and_ a b = Comp (And (a, b))
    let or_ a b  = Comp (Or (a, b))
    let imp a b  = Comp (Imp (a, b))

    module Infix = struct
      let (!!) = not_
      let (&&) = and_
      let (||) = or_
      let (=>) = imp
    end
    (* [map_to_list ~elem ~comp t] is a list of the sub-expressions of t derived
       by traversing the AST from the left and apply [elem] to each [elementary]
       expression and [comp] to each compound expression
       TODO Decide if needed/useful *)
    (* let map_to_list : elem:(elementary -> 'a) -> comp:(compound -> 'a) -> t -> 'a list =
     *   fun ~elem ~comp t ->
     *   let rec map = function
     *     | Elem e -> [elem e]
     *     | Comp c -> comp c :: map_compound c
     *   and map_compound = function
     *     | Not t -> map t
     *     | And (a, b) | Or (a, b) | Imp (a, b) -> map a @ map b
     *   in
     *   map t *)

    (** "The number of logical symbols occurring in a formula is called {i the degree
        of the formula}. (71)" *)
    let rec degree : t -> int = function
      | Elem _ -> 0
      | Comp c -> 1 + compound_degree c
    and compound_degree : compound -> int = function
      | Not f -> degree f
      | And (a, b) | Or (a, b) | Imp (a, b) -> degree a + degree b

    let terminal_symbol_of_compound : compound -> Symbol.logic = function
      | Not _ -> Symbol.Not
      | And _ -> Symbol.And
      | Or _  -> Symbol.Or
      | Imp _ -> Symbol.Imp

    (** "The logical symbol of a nonelementary formula that has been adjoined last
        in the construction of the formula ... is called the {i terminal symbol of the
        formula}." (71) *)
    let terminal_symbol : t -> Symbol.logic option = function
      | Elem _ -> None
      | Comp c -> Some (terminal_symbol_of_compound c)

    let rec subformulas : t -> t list = function
      | Elem _ as t -> [t]
      | Comp c as t -> t :: compound_subformulas c
    and compound_subformulas : compound -> t list = function
      | Not t -> subformulas t
      | And (a, b) | Or (a, b) | Imp (a, b) -> subformulas a @ subformulas b
  end

  (* TODO Sequents? *)
end

module Figure = struct

  (* NOTE The same definition of figure works for sequents, if we end up wanting
     to handle those, we can just functorize the Figure module over an
     expression. *)
  open Expression

  (** The formula which compose a derivation so defined are called {i
      D-formulae} (i.e., derivation formulae). By this we wish to indicate that
      we are not considering merely the formula as such, but also its position
      in the derivation.
      ...
      Thus by 'A is the {i same} {i D}-formula as B' we mean that A and B are
      not only formally identical, but occur also in the same place in the
      derivation. We shall use the words 'formally identical' to indicate
      identity of form  regardless of place. *)
  type upper = Formula.t list
  type lower = Formula.t
  type endformula = lower

  type t =
    | Initial of Formula.t
    (** "The initial formulae of a derivation are {i assumption formulae}"*)
    | Deriv of t list * Formula.t
    (** "A {i proof figure}, called a {i derivation} for short..." (72) *)


  (** "A {i path} in a derivation is a sequence of {i D}-formula whose first
      formula is an initial formula and whose last formula is the endformula,
      and of which each formula except the last is an upper formula of a
      {i D}-inference figure whose lower formula is the next formula in the
      path." *)

  (* let path TODO ?*)

  let endformula : t -> Formula.t = function
    | Initial c -> c
    | Deriv (_, c) -> c

  let initial f = Initial f
  let deriv derivs conclusion = Deriv (derivs, conclusion)

  let rec initial_formulae : t -> Formula.t list = function
    | Initial c -> [c]
    | Deriv (d, _) ->
      List.map ~f:initial_formulae d
      |> ListLabels.flatten

  (* let ex =
   *   let a = Formula.prop "A"
   *   and b = Formula.prop "B"
   *   in
   *   Deriv ([Deriv ([Initial a; Initial b],
   *                  Formula.and_ a b);       Initial b],
   *          Formula.and_ (Formula.and_ a b) b) *)

  (*   (\** 3.1 - 3.2 *\)
   * type t =
   *   | Infer of upper * lower
   *   (\** "A {i proof figure}, called a {i derivation} for short..." (72) *\)
   *   | Deriv of t list * endformula *)

  let ie = ie "finite sets of symbols" 70
end
