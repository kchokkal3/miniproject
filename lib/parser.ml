open! Core
open Or_error.Let_syntax
open Token
open Expr

module State = struct
  module Item = struct
    type t = Token of LToken.t | Expr of LExpr.t [@@deriving equal, sexp]
  end

  type t = Item.t Stack.t Or_error.t * LToken.t Queue.t [@@deriving sexp]

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

  let reduce_expr (state : t) ~(f : Item.t Stack.t -> unit Or_error.t)
      ~(after : Item.t Stack.t -> unit) =
    let stack_err, _ = state in
    let%bind stack = stack_err in
    let%bind () = f stack in
    let () = after stack in
    Ok ()

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

let get_name_lambda tok =
  match tok with LToken.Name x -> Ok x | _ -> Or_error.error_string "Not Name"

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

let rec reduce_lambda (stack : State.Item.t Stack.t) =
  match Stack.pop stack with
  | None -> ()
  | Some (Expr f_expr) -> (
      match Stack.pop stack with
      | None -> Stack.push stack (Expr f_expr)
      | Some (Expr s_expr) ->
          Stack.push stack (Expr (LExpr.App (f_expr, s_expr)));
          reduce_lambda stack
      | Some s_item ->
          Stack.push stack s_item;
          Stack.push stack (Expr f_expr))
  | Some item -> Stack.push stack item

let rec parse_expr_lambda state =
  match State.peek state with
  | None -> Ok ()
  | Some tok -> (
      match tok with
      | LToken.LParen ->
          let%bind () = parse_tok_lambda state LToken.LParen in
          let%bind () = parse_expr_lambda state in
          let%bind () = parse_tok_lambda state LToken.RParen in
          State.reduce_expr state ~after:reduce_lambda ~f:(fun stack ->
              let%bind _ = State.pop_and_match_token stack LToken.RParen in
              let%bind expr = State.pop_expr stack in
              let%bind _ = State.pop_and_match_token stack LToken.LParen in
              Ok (Stack.push stack (Expr expr)))
      | LToken.BSlash ->
          let%bind () =
            parse_tok_list_lambda state [ BSlash; Name ""; Period ]
          in
          let%bind () = parse_expr_lambda state in
          State.reduce_expr state ~after:reduce_lambda ~f:(fun stack ->
              let%bind expr = State.pop_expr stack in
              let%bind _ = State.pop_and_match_token stack LToken.Period in
              let%bind n = State.pop_and_match_token stack (LToken.Name "") in
              let%bind name = get_name_lambda n in
              let%bind _ = State.pop_and_match_token stack LToken.BSlash in
              Ok (Stack.push stack (Expr (LExpr.Lam (name, expr)))))
      | x when LToken.equal x (LToken.Name "") ->
          let%bind () = parse_tok_list_lambda state [ Name "" ] in
          let%bind () = parse_expr_lambda state in
          State.reduce_expr state ~after:reduce_lambda ~f:(fun stack ->
              let%bind n = State.pop_and_match_token stack (LToken.Name "") in
              let%bind name = get_name_lambda n in
              Ok (Stack.push stack (Expr (LExpr.Var name))))
      | _ -> Or_error.error_s (State.sexp_of_t state))

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
                let%bind name = get_name_lambda n in
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
  let print_sexp sexp ~tag =
    print_endline [%string "%{tag}: %{Sexp.to_string_hum sexp}"]
  in
  let parse_and_print s =
    let toks = Lexer.lex_lambda s in
    match toks with
    | Error e -> Error.sexp_of_t e |> print_sexp ~tag:"Error in lexing"
    | Ok toks -> (
        let progs = parse_lambda toks in
        match progs with
        | Error e -> Error.sexp_of_t e |> print_sexp ~tag:"Error in parsing"
        | Ok progs ->
            List.iter progs ~f:(fun prog ->
                LProg.sexp_of_t prog |> print_sexp ~tag:"Program"))
  in
  let _ = parse_and_print "defn I = \\x. x" in
  ();
  [%expect {| Program: (Defn I (Lam x (Var x))) |}]
