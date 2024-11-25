open! Core
open Or_error.Let_syntax
open Token

module State = struct
  type t = LToken.t Stack.t Or_error.t

  let init () = Ok (Stack.of_list [])

  let shift (state : t) ~(tok : LToken.t) =
    let%bind stack = state in
    Ok (Stack.push stack tok)

  let reduce (state : t) ~(f : LToken.t Stack.t -> LToken.t Stack.t) =
    let%bind stack = state in
    Ok (f stack)
end

let parse_expr_lambda toks = Error.of_string "Not implemented"
let parse_prog_lambda toks = Error.of_string "Not implemented"
