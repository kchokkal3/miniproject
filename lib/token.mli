module LToken : sig
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

module FToken : sig
  type t = LParen | RParen | BSlash | Name of string
end
