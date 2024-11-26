open! Core
open Or_error.Let_syntax
open Token
open Expr

module State = struct
  module Item = struct
    type t = Token of LToken.t | Expr of LExpr.t [@@deriving equal]
  end

  type t = Item.t Stack.t Or_error.t * LToken.t Queue.t

  let init toks = (Ok (Stack.of_list []), Queue.of_list toks)

  let peek (state : t) =
    let _, toks = state in
    Queue.peek toks

  let shift_tok (state : t) =
    let stack_err, toks = state in
    let%bind stack = stack_err in
    match Queue.dequeue toks with
    | None -> Or_error.error_string "Nothing to shift"
    | Some tok -> Ok (Stack.push stack (Token tok))

  let _shift_expr (state : t) ~(f : LToken.t -> LExpr.t) =
    let stack_err, toks = state in
    let%bind stack = stack_err in
    match Queue.dequeue toks with
    | None -> Or_error.error_string "Nothing to shift"
    | Some tok -> Ok (Stack.push stack (Expr (f tok)))

  let reduce_prog (state : t) ~(f : Item.t Stack.t -> LProg.t Or_error.t) =
    let stack_err, _ = state in
    let%bind stack = stack_err in
    let%bind item = f stack in
    match Stack.is_empty stack with
    | true -> Ok item
    | false -> Or_error.error_string "Reducing to prog leaves items on stack"

  let _reduce_expr (state : t) ~(f : Item.t Stack.t -> unit Or_error.t) =
    let stack_err, _ = state in
    let%bind stack = stack_err in
    let%bind item = f stack in
    Ok item

  let pop_expr (stack : Item.t Stack.t) =
    match Stack.pop stack with
    | Some stack_item -> (
        match stack_item with
        | Expr e -> Ok e
        | _ -> Or_error.error_string "Item types didn't match")
    | None -> Or_error.error_string "Nothing to pop"

  let pop_token (stack : Item.t Stack.t) =
    match Stack.pop stack with
    | Some stack_item -> (
        match stack_item with
        | Token e -> Ok e
        | _ -> Or_error.error_string "Item types didn't match")
    | None -> Or_error.error_string "Nothing to pop"

  let pop_and_match_token (stack : Item.t Stack.t) (tok : LToken.t) =
    let%bind state_tok = pop_token stack in
    match LToken.equal state_tok tok with
    | true -> Ok state_tok
    | false -> Or_error.error_string "Token didn't match"
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
      | true -> State.shift_tok state)

let rec parse_tok_list_lambda state toks =
  match toks with
  | [] -> Ok ()
  | tok :: toks ->
      Or_error.bind (parse_tok_lambda state tok) ~f:(fun () ->
          parse_tok_list_lambda state toks)

let parse_expr_lambda _state = Or_error.error_string "Not implemented"

let rec parse_prog_lambda state =
  match State.peek state with
  | None -> Ok []
  | Some tok -> (
      match tok with
      | LToken.Defn ->
          let%bind () =
            parse_tok_list_lambda state
              [ LToken.Defn; LToken.Name ""; LToken.Equal ]
          in
          let%bind () = parse_expr_lambda state in
          let%bind prog =
            State.reduce_prog state ~f:(fun stack ->
                let%bind expr = State.pop_expr stack in
                let%bind _ = State.pop_and_match_token stack LToken.Equal in
                let%bind n = State.pop_and_match_token stack (LToken.Name "") in
                let name =
                  match n with
                  | LToken.Name x -> x
                  | _ -> invalid_arg "Impossible case of name"
                in
                let%bind _ = State.pop_and_match_token stack LToken.Defn in
                Ok (LProg.Defn (name, expr)))
          in
          let%bind progs = parse_prog_lambda state in
          Ok (prog :: progs)
      | LToken.Eval ->
          let%bind () =
            parse_tok_list_lambda state
              [ LToken.Eval; LToken.Name ""; LToken.Equal ]
          in
          let%bind () = parse_expr_lambda state in
          let%bind prog =
            State.reduce_prog state ~f:(fun stack ->
                let%bind expr = State.pop_expr stack in
                let%bind _ = State.pop_and_match_token stack LToken.Equal in
                let%bind _ = State.pop_and_match_token stack LToken.Eval in
                Ok (LProg.Eval expr))
          in
          let%bind progs = parse_prog_lambda state in
          Ok (prog :: progs)
      | _ -> Or_error.error_string "Not Defn or Eval")

let parse_lambda toks =
  let state = State.init toks in
  parse_prog_lambda state

let%expect_test _ =
  let _ =
    let toks = Lexer.lex_lambda "defn I = \\x. x" in
    match toks with
    | Error e -> Error.sexp_of_t e |> Sexp.to_string |> print_endline
    | Ok toks -> (
        let progs = parse_lambda toks in
        match progs with
        | Error e -> Error.sexp_of_t e |> Sexp.to_string |> print_endline
        | Ok progs ->
            List.iter progs ~f:(fun prog ->
                LProg.sexp_of_t prog |> Sexp.to_string |> print_endline))
  in
  ();
  [%expect {||}]
