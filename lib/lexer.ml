open! Core
open Token

let rec lex_lambda_helper s line col =
  match s with
  | [] -> []
  | h :: tl -> (
      match h with
      | '(' -> LToken.LParen :: lex_lambda_helper tl line (col + 1)
      | ')' -> LToken.RParen :: lex_lambda_helper tl line (col + 1)
      | '\\' -> LToken.BSlash :: lex_lambda_helper tl line (col + 1)
      | '.' -> LToken.Period :: lex_lambda_helper tl line (col + 1)
      | '\n' -> lex_lambda_helper tl (line + 1) 0
      | w when Char.is_whitespace w -> lex_lambda_helper tl line (col + 1)
      (*TODO: Parse multichar names*)
      | x when Char.is_alpha x ->
          LToken.Name (Char.to_string x) :: lex_lambda_helper tl line (col + 1)
      | t -> invalid_arg (Char.to_string t))

let lex_lambda s = lex_lambda_helper (String.to_list s) 0 0

let%expect_test _ =
  let lex_and_print x =
    lex_lambda x |> List.map ~f:LToken.sexp_of_t |> fun x ->
    Sexp.List x |> Sexp.to_string |> print_endline
  in
  lex_and_print "()";
  [%expect {| (LParen RParen) |}];
  lex_and_print "";
  [%expect {| () |}];
  lex_and_print "(\\x. x)";
  [%expect {| (LParen BSlash(Name x)Period(Name x)RParen) |}]
