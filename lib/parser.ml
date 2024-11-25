open! Core
open Or_error.Let_syntax
open Token
open Expr

module State = struct
  type t = LToken.t Stack.t Or_error.t * LToken.t List.t

  let init toks = (Ok (Stack.of_list []), toks)

  let peek t =
    let _, toks = t in
    match toks with [] -> None | tok :: _ -> Some tok

  let shift (state : t) =
    let stack_err, toks = state in
    let%bind stack = stack_err in
    match List.hd toks with
    | None -> Or_error.error_string "Nothing to shift"
    | Some tok -> Ok (Stack.push stack tok)

  let reduce (state : t) ~(f : LToken.t Stack.t -> unit Or_error.t) =
    let stack_err, _ = state in
    let%bind stack = stack_err in
    let%bind () = f stack in
    Ok ()
end

let parse_tok_lambda state tok =
  match State.peek state with
  | None -> Or_error.error_string "Nothing to parse"
  | Some state_tok -> (
      match LToken.equal state_tok tok with
      | false ->
          Or_error.error_string
            [%string
              "Trying to parse %{LToken.sexp_of_t tok#Sexp} but found \
               %{LToken.sexp_of_t state_tok#Sexp}"]
      | true -> State.shift state)

let rec parse_tok_list_lambda state toks =
  match toks with
  | [] -> Ok ()
  | tok :: toks ->
      Or_error.bind (parse_tok_lambda state tok) ~f:(fun () ->
          parse_tok_list_lambda state toks)

let parse_expr_lambda state = Or_error.error_string "Not implemented"

let parse_prog_lambda state =
  match State.peek state with
  | None -> Or_error.error_string "Invalid parse"
  | Some tok -> (
      match tok with
      | LToken.Defn ->
          let%bind () =
            parse_tok_list_lambda state
              [ LToken.Defn; LToken.Name ""; LToken.Equal ]
          in
          Ok ()
      | LToken.Eval ->
          let%bind () =
            parse_tok_list_lambda state
              [ LToken.Defn; LToken.Name ""; LToken.Equal ]
          in
          let%bind () = parse_expr_lambda state in
          State.reduce state ~f:(fun stack ->
              match Stack.pop stack with
              | Some expr -> (
                  match Stack.pop stack with
                  | Some LToken.Equal -> (
                      match Stack.pop stack with
                      | Some (LToken.Name x) -> (
                          match Stack.pop stack with
                          | Some LToken.Defn ->
                              Stack.push stack (LProg.Defn (x, expr))
                          | _ -> Or_error.error_string "Expecting Defn")
                      | _ -> Or_error.error_string "Expecting Name")
                  | _ -> Or_error.error_string "Expecting Equal")
              | None -> Or_error.error_string "Expecting expr")
      | _ -> Or_error.error_string "Not Defn or Eval")

let parse_lambda toks =
  let state = State.init toks in
  parse_prog_lambda state
