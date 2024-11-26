open Core
open Token
open Expr

val parse_lambda : LToken.t list -> LProg.t List.t Or_error.t
