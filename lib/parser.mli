open Core
open Token
open Expr

val parse_expr_lambda : LToken.t list -> LExpr.t Or_error.t
val parse_prog_lambda : LToken.t list -> LProg.t list Or_error.t
