open! Core
open Token
open Or_error.Let_syntax

let check_reserved_lambda s =
  match s with "defn" -> Tok.Defn | "eval" -> Tok.Eval | _ -> Tok.Name s

let rec lex_helper_lambda s (line : int) col =
  match s with
  | [] -> Ok []
  | h :: tl -> (
      let construct_token tok rest = Ok (Token.init tok (line, col) :: rest) in
      match h with
      | '(' ->
          let%bind rest = lex_helper_lambda tl line (col + 1) in
          construct_token Tok.LParen rest
      | ')' ->
          let%bind rest = lex_helper_lambda tl line (col + 1) in
          construct_token Tok.RParen rest
      | '\\' ->
          let%bind rest = lex_helper_lambda tl line (col + 1) in
          construct_token Tok.BSlash rest
      | '.' ->
          let%bind rest = lex_helper_lambda tl line (col + 1) in
          construct_token Tok.Period rest
      | '=' ->
          let%bind rest = lex_helper_lambda tl line (col + 1) in
          construct_token Tok.Equal rest
      | '%' when Int.equal col 0 -> (
          let tl =
            List.drop_while tl ~f:(fun c -> not (Char.equal c '\n')) |> List.tl
          in
          match tl with
          | Some tl -> lex_helper_lambda tl (line + 1) 0
          | None -> Ok [])
      | '\n' -> lex_helper_lambda tl (line + 1) 0
      | w when Char.is_whitespace w -> lex_helper_lambda tl line (col + 1)
      | x when Char.is_alpha x ->
          let name =
            String.take_while
              ~f:(fun c ->
                Char.is_alpha c || Char.is_digit c || Char.equal '_' c)
              (String.of_list s)
          in
          let name_len = String.length name in
          let new_tl = List.drop s name_len in
          let%bind rest = lex_helper_lambda new_tl line (col + name_len) in
          construct_token (check_reserved_lambda name) rest
      | t ->
          Or_error.error_string
            [%string
              "Invalid token '%{t#Char}' at line %{line#Int}, column %{col#Int}"]
      )

let lex_lambda s = lex_helper_lambda (String.to_list s) 1 1

let%expect_test _ =
  let lex_and_print x =
    lex_lambda x
    |> Or_error.sexp_of_t (fun l -> Sexp.List (List.map ~f:Token.sexp_of_t l))
    |> print_s
  in
  lex_and_print "()";
  [%expect {| (Ok ((LParen 1 1) (RParen 1 2))) |}];
  lex_and_print "";
  [%expect {| (Ok ()) |}];
  lex_and_print "(\\x. x)";
  [%expect
    {|
    (Ok
     ((LParen 1 1) (BSlash 1 2) ((Name x) 1 3) (Period 1 4) ((Name x) 1 6)
      (RParen 1 7)))
    |}];
  lex_and_print "$";
  [%expect {| (Error "Invalid token '$' at line 1, column 1") |}];
  lex_and_print "\\x. \\y. x y\n \\y. 2";
  [%expect {| (Error "Invalid token '2' at line 2, column 5") |}];
  lex_and_print "(\\test. test)";
  [%expect
    {|
    (Ok
     ((LParen 1 1) (BSlash 1 2) ((Name test) 1 3) (Period 1 7) ((Name test) 1 9)
      (RParen 1 13)))
    |}];
  lex_and_print
    {|defn Y = \h. (\x. h (x x)) (\x. h (x x))
    defn I = \x. x
    eval Y I
    |};
  [%expect
    {|
    (Ok
     ((Defn 1 1) ((Name Y) 1 6) (Equal 1 8) (BSlash 1 10) ((Name h) 1 11)
      (Period 1 12) (LParen 1 14) (BSlash 1 15) ((Name x) 1 16) (Period 1 17)
      ((Name h) 1 19) (LParen 1 21) ((Name x) 1 22) ((Name x) 1 24) (RParen 1 25)
      (RParen 1 26) (LParen 1 28) (BSlash 1 29) ((Name x) 1 30) (Period 1 31)
      ((Name h) 1 33) (LParen 1 35) ((Name x) 1 36) ((Name x) 1 38) (RParen 1 39)
      (RParen 1 40) (Defn 2 4) ((Name I) 2 9) (Equal 2 11) (BSlash 2 13)
      ((Name x) 2 14) (Period 2 15) ((Name x) 2 17) (Eval 3 4) ((Name Y) 3 9)
      ((Name I) 3 11)))
    |}]
