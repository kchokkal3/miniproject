open! Core

module LExpr = struct
  type t = Var of string | Lam of string * t | App of t * t
  [@@deriving sexp, equal]
end

module LProg = struct
  type t = Defn of string * LExpr.t | Eval of LExpr.t [@@deriving sexp, equal]
end
