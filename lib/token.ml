open! Core

module LToken = struct
  type t =
    | LParen
    | RParen
    | BSlash
    | Period
    | Defn
    | Eval
    | Equal
    | Name of (string[@equal.ignore])
  [@@deriving sexp, equal]
end

module FToken = struct
  type t = LParen | RParen | BSlash | Name of string
end

let%expect_test _ =
  print_endline (Bool.to_string (LToken.equal (Name "x") (Name "y")));
  [%expect {| true |}]
