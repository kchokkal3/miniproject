open! Core
open Or_error.Let_syntax
open Token
open Expr

module State = struct
  module Item = struct
    type t = Token of Token.t | Expr of LExpr.t [@@deriving equal, sexp]
  end

  type t = Item.t Stack.t Or_error.t * Token.t Queue.t [@@deriving sexp]

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

  let shift_expr (state : t) ~(f : Token.t -> LExpr.t Or_error.t) =
    let stack_err, toks = state in
    let%bind stack = stack_err in
    match Queue.dequeue toks with
    | None -> Or_error.error_string "Nothing to shift"
    | Some tok ->
        let%bind expr = f tok in
        Ok (Stack.push stack (Expr expr))

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
        | _ ->
            Or_error.error_string
              [%string "%{Item.sexp_of_t stack_item#Sexp} isn't an expr"])
    | None -> Or_error.error_string "Nothing to pop"

  let pop_token (stack : Item.t Stack.t) =
    match Stack.pop stack with
    | Some stack_item -> (
        match stack_item with
        | Token e -> Ok e
        | _ ->
            Or_error.error_string
              [%string "%{Item.sexp_of_t stack_item#Sexp} isn't a token"])
    | None -> Or_error.error_string "Nothing to pop"

  let pop_and_match_token (stack : Item.t Stack.t) (tok : Tok.t) =
    let%bind state_tok = pop_token stack in
    match Tok.equal (Token.get_tok state_tok) tok with
    | true -> Ok state_tok
    | false ->
        Or_error.error_string
          [%string
            "Trying to parse %{Tok.sexp_of_t tok#Sexp} but found \
             %{Token.sexp_of_t state_tok#Sexp}"]
end

let get_name tok =
  match Token.get_tok tok with
  | Tok.Name x -> Ok x
  | _ -> Or_error.error_string "Not Name"

let parse_tok state (tok : Tok.t) =
  match State.peek state with
  | None -> Or_error.error_string "Nothing to parse"
  | Some state_tok -> (
      match Tok.equal (Token.get_tok state_tok) tok with
      | false ->
          Or_error.error_string
            [%string
              "Trying to parse %{Tok.sexp_of_t tok#Sexp} but found \
               %{Token.sexp_of_t state_tok#Sexp}"]
      | true -> State.shift_tok state)

let rec parse_tok_list state toks =
  match toks with
  | [] -> Ok ()
  | tok :: toks ->
      Or_error.bind (parse_tok state tok) ~f:(fun () ->
          parse_tok_list state toks)

let rec reduce_lambda (stack : State.Item.t Stack.t) =
  match Stack.pop stack with
  | None -> ()
  | Some (Expr f_expr) -> (
      match Stack.pop stack with
      | None -> Stack.push stack (Expr f_expr)
      | Some (Expr s_expr) ->
          Stack.push stack (Expr (LExpr.App (s_expr, f_expr)));
          reduce_lambda stack
      | Some s_item ->
          Stack.push stack s_item;
          Stack.push stack (Expr f_expr))
  | Some item -> Stack.push stack item

let rec parse_expr_lambda state =
  let%bind () =
    State.reduce_expr state
      ~f:(Or_error.return () |> const)
      ~after:reduce_lambda
  in
  (* let () = State.sexp_of_t state |> print_s in *)
  match State.peek state with
  | None -> Ok ()
  | Some tok -> (
      match Token.get_tok tok with
      | Tok.LParen ->
          let%bind () = parse_tok state Tok.LParen in
          let%bind () = parse_expr_lambda state in
          let%bind () = parse_tok state Tok.RParen in
          let%bind () =
            State.reduce_expr state ~after:reduce_lambda ~f:(fun stack ->
                let%bind _ = State.pop_and_match_token stack Tok.RParen in
                let%bind expr = State.pop_expr stack in
                let%bind _ = State.pop_and_match_token stack Tok.LParen in
                Ok (Stack.push stack (Expr expr)))
          in
          parse_expr_lambda state
      | Tok.BSlash ->
          let%bind () = parse_tok_list state [ BSlash; Name ""; Period ] in
          let%bind () = parse_expr_lambda state in
          State.reduce_expr state ~after:reduce_lambda ~f:(fun stack ->
              let%bind expr = State.pop_expr stack in
              let%bind _ = State.pop_and_match_token stack Tok.Period in
              let%bind n = State.pop_and_match_token stack (Tok.Name "") in
              let%bind name = get_name n in
              let%bind _ = State.pop_and_match_token stack Tok.BSlash in
              Ok (Stack.push stack (Expr (LExpr.Lam (name, expr)))))
      | x when Tok.equal x (Tok.Name "") ->
          let%bind () =
            State.shift_expr state ~f:(fun tok ->
                match Token.get_tok tok with
                | Name x -> Ok (LExpr.Var x)
                | _ ->
                    Or_error.error_string
                      [%string
                        "Expecting %{Token.sexp_of_t tok#Sexp} to be a Name"])
          in
          parse_expr_lambda state
      | _ -> Ok ())

let rec parse_prog_lambda state =
  match State.peek state with
  | None -> Ok []
  | Some tok -> (
      match Token.get_tok tok with
      | Tok.Defn ->
          let%bind () =
            parse_tok_list state [ Tok.Defn; Tok.Name ""; Tok.Equal ]
          in
          let%bind () = parse_expr_lambda state in
          let%bind prog =
            State.reduce_prog state ~f:(fun stack ->
                let%bind expr = State.pop_expr stack in
                let%bind _ = State.pop_and_match_token stack Tok.Equal in
                let%bind n = State.pop_and_match_token stack (Tok.Name "") in
                let%bind name = get_name n in
                let%bind _ = State.pop_and_match_token stack Tok.Defn in
                Ok (LProg.Defn (name, expr)))
          in
          let%bind progs = parse_prog_lambda state in
          Ok (prog :: progs)
      | Tok.Eval ->
          let%bind () = parse_tok_list state [ Tok.Eval ] in
          let%bind () = parse_expr_lambda state in
          let%bind prog =
            State.reduce_prog state ~f:(fun stack ->
                let%bind expr = State.pop_expr stack in
                let%bind _ = State.pop_and_match_token stack Tok.Eval in
                Ok (LProg.Eval expr))
          in
          let%bind progs = parse_prog_lambda state in
          Ok (prog :: progs)
      | _ ->
          Or_error.error_string
            [%string "Expecting Defn or Eval: %{State.sexp_of_t state#Sexp}"])

let parse_lambda toks =
  let state = State.init toks in
  parse_prog_lambda state

let%expect_test _ =
  let print_sexp sexp ~tag =
    print_endline [%string "%{tag}:\n %{Sexp.to_string_hum sexp}"]
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
  [%expect {|
    Program:
     (Defn I (Lam x (Var x)))
    |}];
  let _ = parse_and_print "defn I = \\x. x x" in
  ();
  [%expect {|
    Program:
     (Defn I (Lam x (App (Var x) (Var x))))
    |}];
  let _ = parse_and_print "defn I = \\h. (\\x. h x x) (\\x. h x x)" in
  ();
  [%expect
    {|
    Program:
     (Defn I
     (Lam h
      (App (Lam x (App (App (Var h) (Var x)) (Var x)))
       (Lam x (App (App (Var h) (Var x)) (Var x))))))
    |}];

  let _ =
    parse_and_print
      "defn Y = \\h. (\\x. h x x) (\\x. h x x)\n defn I = \\x. x\n eval Y I"
  in
  ();
  [%expect
    {|
    Program:
     (Defn Y
     (Lam h
      (App (Lam x (App (App (Var h) (Var x)) (Var x)))
       (Lam x (App (App (Var h) (Var x)) (Var x))))))
    Program:
     (Defn I (Lam x (Var x)))
    Program:
     (Eval (App (Var Y) (Var I)))
        |}];
  let _ =
    parse_and_print
      {|
  defn zero = \s. \z. z
  defn succ = \n. \s. \z. s (n s z)
defn plus = \n. \k. n succ k
defn times = \n. \k. n (plus k) zero
defn exp = \n. \k. k (times n) one

defn pair = \x. \y. \k. k x y
defn pred2 = \n. n (\p. p (\x. \y. pair (succ x) x)) (pair zero zero)

defn pred = \n. pred2 n (\x. \y. y)
defn Y = \h. (\x. h (x x)) (\x. h (x x))
% Inspiration for how to structure general recursive functions from lecture notes
defn h_lucas = \f. \n. if0 n two (if1 n one (plus (f (two pred n)) (f (one pred n))))
defn lucas = Y h_lucas
        |}
  in
  ();
  [%expect
    {|
    Program:
     (Defn zero (Lam s (Lam z (Var z))))
    Program:
     (Defn succ
     (Lam n (Lam s (Lam z (App (Var s) (App (App (Var n) (Var s)) (Var z)))))))
    Program:
     (Defn plus (Lam n (Lam k (App (App (Var n) (Var succ)) (Var k)))))
    Program:
     (Defn times
     (Lam n (Lam k (App (App (Var n) (App (Var plus) (Var k))) (Var zero)))))
    Program:
     (Defn exp
     (Lam n (Lam k (App (App (Var k) (App (Var times) (Var n))) (Var one)))))
    Program:
     (Defn pair (Lam x (Lam y (Lam k (App (App (Var k) (Var x)) (Var y))))))
    Program:
     (Defn pred2
     (Lam n
      (App
       (App (Var n)
        (Lam p
         (App (Var p)
          (Lam x (Lam y (App (App (Var pair) (App (Var succ) (Var x))) (Var x)))))))
       (App (App (Var pair) (Var zero)) (Var zero)))))
    Program:
     (Defn pred (Lam n (App (App (Var pred2) (Var n)) (Lam x (Lam y (Var y))))))
    Program:
     (Defn Y
     (Lam h
      (App (Lam x (App (Var h) (App (Var x) (Var x))))
       (Lam x (App (Var h) (App (Var x) (Var x)))))))
    Program:
     (Defn h_lucas
     (Lam f
      (Lam n
       (App (App (App (Var if0) (Var n)) (Var two))
        (App (App (App (Var if1) (Var n)) (Var one))
         (App
          (App (Var plus) (App (Var f) (App (App (Var two) (Var pred)) (Var n))))
          (App (Var f) (App (App (Var one) (Var pred)) (Var n)))))))))
    Program:
     (Defn lucas (App (Var Y) (Var h_lucas)))
    |}]
