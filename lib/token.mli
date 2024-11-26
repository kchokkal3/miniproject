module Tok : sig
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

val equal : t -> t -> bool
val get_tok : t -> Tok.t
val get_info : t -> int * int
val init : Tok.t -> int * int -> t
