open! Core

module Tok = struct
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

type t = Tok.t * int * int [@@deriving sexp]

let equal (f, _, _) (s, _, _) = Tok.equal f s
let get_tok (t, _, _) = t
let get_info (_, l, c) = (l, c)
let init tok (l, c) = (tok, l, c)

let%expect_test _ =
  print_endline (Bool.to_string (Tok.equal (Name "x") (Name "y")));
  [%expect {| true |}]
