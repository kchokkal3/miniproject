module LToken : sig
  type t =
    | LParen
    | RParen
    | BSlash
    | Period
    | Defn
    | Eval
    | Equal
    | Name of string
  [@@deriving sexp]
end

module FToken : sig
  type t = LParen | RParen | BSlash | Name of string
end
