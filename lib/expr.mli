open! Core

module LExpr : sig
  type t = Var of string | Lam of string * t | App of t * t [@@deriving sexp]
end

module LProg : sig
  type t = Defn of string * LExpr.t | Eval of LExpr.t [@@deriving sexp]
end
