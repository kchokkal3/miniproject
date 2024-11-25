open! Core

module LToken = struct
  type t = LParen | RParen | BSlash | Period | Name of string
  [@@deriving sexp]
end

module FToken = struct
  type t = LParen | RParen | BSlash | Name of string
end
