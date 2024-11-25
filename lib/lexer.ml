open! Core
open Token
open Or_error.Let_syntax

let check_reserved_lambda s =
  match s with
  | "defn" -> LToken.Defn
  | "eval" -> LToken.Eval
  | _ -> LToken.Name s

let rec lex_helper_lambda s line col =
  match s with
  | [] -> Ok []
  | h :: tl -> (
      match h with
      | '(' ->
          let%bind rest = lex_helper_lambda tl line (col + 1) in
          Ok (LToken.LParen :: rest)
      | ')' ->
          let%bind rest = lex_helper_lambda tl line (col + 1) in
          Ok (LToken.RParen :: rest)
      | '\\' ->
          let%bind rest = lex_helper_lambda tl line (col + 1) in
          Ok (LToken.BSlash :: rest)
      | '.' ->
          let%bind rest = lex_helper_lambda tl line (col + 1) in
          Ok (LToken.Period :: rest)
      | '=' ->
          let%bind rest = lex_helper_lambda tl line (col + 1) in
          Ok (LToken.Equal :: rest)
      | '\n' -> lex_helper_lambda tl (line + 1) 0
      | w when Char.is_whitespace w -> lex_helper_lambda tl line (col + 1)
      | x when Char.is_alpha x ->
          let name = String.take_while ~f:Char.is_alpha (String.of_list s) in
          let name_len = String.length name in
          let new_tl = List.drop s name_len in
          let%bind rest = lex_helper_lambda new_tl line (col + name_len) in
          Ok (check_reserved_lambda name :: rest)
      | t ->
          Or_error.error_string
            [%string
              "Invalid token '%{t#Char}' at line %{line#Int}, column %{col#Int}"]
      )

let lex_lambda s = lex_helper_lambda (String.to_list s) 0 0

let%expect_test _ =
  let lex_and_print x =
    lex_lambda x
    |> Or_error.sexp_of_t (fun l -> Sexp.List (List.map ~f:LToken.sexp_of_t l))
    |> Sexp.to_string |> print_endline
  in
  lex_and_print "()";
  [%expect {| (Ok(LParen RParen)) |}];
  lex_and_print "";
  [%expect {| (Ok()) |}];
  lex_and_print "(\\x. x)";
  [%expect {| (Ok(LParen BSlash(Name x)Period(Name x)RParen)) |}];
  lex_and_print "$";
  [%expect {| (Error"Invalid token '$' at line 0, column 0") |}];
  lex_and_print "\\x. \\y. x y\n \\y. 2";
  [%expect {| (Error"Invalid token '2' at line 1, column 5") |}];
  lex_and_print "(\\test. test)";
  [%expect {| (Ok(LParen BSlash(Name test)Period(Name test)RParen)) |}];
  lex_and_print
    {|defn Y = \h. (\x. h (x x)) (\x. h (x x))
    defn I = \x. x
    eval Y I
    |};
  [%expect
    {| (Ok(Defn(Name Y)Equal BSlash(Name h)Period LParen BSlash(Name x)Period(Name h)LParen(Name x)(Name x)RParen RParen LParen BSlash(Name x)Period(Name h)LParen(Name x)(Name x)RParen RParen Defn(Name I)Equal BSlash(Name x)Period(Name x)Eval(Name Y)(Name I))) |}]
