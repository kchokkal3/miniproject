open Core

module Typ : sig
  module TVar : sig
    type t = string
  end

  module TNat : sig
    type t
  end

  module TBool : sig
    type t
  end

  module TProd : sig
    type 'a t = 'a * 'a
  end

  module TSum : sig
    type ('a, 'b) t = ('a, 'b) Either.t
  end

  module TUnit : sig
    type t
  end

  module TVoid : sig
    type t
  end

  module TArrow : sig
    type ('a, 'b) t = 'a -> 'b
  end

  module TRec : sig
    type 'a t = string * 'a
  end

  type t =
    | Var of TVar.t
    | Nat of TNat.t
    | Bool of TBool.t
    | Prod of t TProd.t
    | Sum of (t, t) TSum.t
    | Unit of TUnit.t
    | Void of TUnit.t
    | Rec of t TRec.t
end

module Expr : sig
  type t
end

module TypExpr : sig
  type ('a, 'b) eq = Refl : ('a, 'a) eq

  type _ t =
    | Var : string -> Typ.TVar.t t
    | Nat : int -> Typ.TNat.t t
    | Bool : bool -> Typ.TBool.t t
    | Succ : Typ.TNat.t t -> Typ.TNat.t t
    | Ifz : (Typ.TNat.t t * 'a t * 'a t) -> 'a t
    | Lam : ('b t * string * 'a t) -> ('b, 'a) Typ.TArrow.t t
    | App : (('a, 'b) Typ.TArrow.t * 'a t) -> 'b t
    | Fold : ('a Typ.TRec.t * 'b t) -> 'a Typ.TRec.t t
    | Unfold : 'a Typ.TRec.t t -> 'b t
    | Prod : ('a t * 'a t) -> 'a Typ.TProd.t t
    | Unit : unit -> Typ.TUnit.t t
    | PrLeft : 'a Typ.TProd.t -> 'a t
    | PrRight : 'a Typ.TProd.t -> 'a t
    | InLeft : ('a t * Typ.t) -> ('a, 'b) Typ.TSum.t t
    | InRight : (Typ.t * 'b t) -> ('a, 'b) Typ.TSum.t t
    | Case : (('a, 'b) Typ.TSum.t * 'c t * 'c t) -> 'c t
    | Abort : ('a * Typ.TVoid.t t) -> 'a t

  val to_string : 'a t -> string
end
