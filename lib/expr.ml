open! Core

module LExpr = struct
  type t = Var of string | Lam of string * t | App of t * t
  [@@deriving sexp, equal]

  let rec to_string t =
    match t with
    | Var x -> x
    | Lam (x, e) -> [%string "\\%{x}. %{to_string e}"]
    | App (f, s) -> [%string "(%{to_string f}) (%{to_string s})"]
end

module LProg = struct
  type t = Defn of string * LExpr.t | Eval of LExpr.t [@@deriving sexp, equal]
end
