open! Core
open Expr

val interpret_lambda : LProg.t list -> LExpr.t list Or_error.t
