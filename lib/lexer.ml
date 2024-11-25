open! Core
open Token
open Or_error.Let_syntax

let rec lex_lambda_helper s line col =
  match s with
  | [] -> Ok []
  | h :: tl -> (
      match h with
      | '(' ->
          let%bind rest = lex_lambda_helper tl line (col + 1) in
          Ok (LToken.LParen :: rest)
      | ')' ->
          let%bind rest = lex_lambda_helper tl line (col + 1) in
          Ok (LToken.RParen :: rest)
      | '\\' ->
          let%bind rest = lex_lambda_helper tl line (col + 1) in
          Ok (LToken.BSlash :: rest)
      | '.' ->
          let%bind rest = lex_lambda_helper tl line (col + 1) in
          Ok (LToken.Period :: rest)
      | '\n' -> lex_lambda_helper tl (line + 1) 0
      | w when Char.is_whitespace w -> lex_lambda_helper tl line (col + 1)
      (*TODO: Parse multichar names*)
      | x when Char.is_alpha x ->
          let%bind rest = lex_lambda_helper tl line (col + 1) in
          Ok (LToken.Name (Char.to_string x) :: rest)
      | t ->
          Or_error.error_string
            [%string
              "Invalid token '%{t#Char}' at line %{line#Int}, column %{col#Int}"]
      )

let lex_lambda s = lex_lambda_helper (String.to_list s) 0 0

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
  [%expect {| (Error"Invalid token '$' at 0, 0") |}];
  lex_and_print "\\x. \\y. x y\n \\y. 2";
  [%expect {| (Error"Invalid token '$' at 0, 0") |}]
