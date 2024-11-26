open Core
open Expr

val parse_lambda : Token.t list -> LProg.t List.t Or_error.t
