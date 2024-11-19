open Core

module Typ = struct
  module TVar = struct
    type t = string

    let to_string v = v
  end

  module TNat = struct
    type t

    let to_string () = "nat"
  end

  module TBool = struct
    type t

    let to_string () = "bool"
  end

  module TProd = struct
    type 'a t = 'a * 'a

    let to_string (p1, p2) = Printf.sprintf "%s * %s" to_str
  end

  module TSum = struct
    type ('a, 'b) t = ('a, 'b) Either.t
  end

  module TUnit = struct
    type t
  end

  module TVoid = struct
    type t
  end

  module TArrow = struct
    type ('a, 'b) t = 'a -> 'b
  end

  module TRec = struct
    type 'a t = string * 'a
  end

  type t =
    | Var of TVar.t
    | Nat of TNat.t
    | Bool of TBool.t
    | Prod of t TProd.t
    | Sum of (t, t) TSum.t
    | Unit of TUnit.t
    | Void of TUnit.t
    | Rec of t TRec.t
end

module Expr = struct
  type t =
    | Var of string
    | Nat of int
    | Bool of bool
    | Succ of t
    | Ifz of t * t * t
    | Lam of string * t
    | App of t * t
    | Fold of Typ.t * t
    | Unfold of t
    | Prod of t * t
    | Unit
    | PrLeft of t
    | PrRight of t
    | Sum of (t, t) Either.t
    | Case of (string * t) list
    | Abort of Typ.t * t
end

module TypExpr = struct
  type ('a, 'b) eq = Refl : ('a, 'a) eq

  type _ t =
    | Var : string -> Typ.TVar.t t
    | Nat : int -> Typ.TNat.t t
    | Bool : bool -> Typ.TBool.t t
    | Succ : Typ.TNat.t t -> Typ.TNat.t t
    | Ifz : (Typ.TNat.t t * 'a t * 'a t) -> 'a t
    | Lam : ('b t * string * 'a t) -> ('b, 'a) Typ.TArrow.t t
    | App : (('a, 'b) Typ.TArrow.t t * 'a t) -> 'b t
    | Fold : ('a Typ.TRec.t * 'b t) -> 'a Typ.TRec.t t
    | Unfold : 'a Typ.TRec.t t -> 'b t
    | Prod : ('a t * 'a t) -> 'a Typ.TProd.t t
    | Unit : unit -> Typ.TUnit.t t
    | PrLeft : 'a Typ.TProd.t -> 'a t
    | PrRight : 'a Typ.TProd.t -> 'a t
    | InLeft : ('a t * Typ.t) -> ('a, 'b) Typ.TSum.t t
    | InRight : (Typ.t * 'b t) -> ('a, 'b) Typ.TSum.t t
    | Case : (('a, 'b) Typ.TSum.t * 'c t * 'c t) -> 'c t
    | Abort : ('a * Typ.TVoid.t t) -> 'a t

  let rec to_string : type a. a t -> string = function
    | Var s -> s
    | Nat n -> Int.to_string n
    | Bool b -> Bool.to_string b
    | Succ e -> String.concat ~sep:"" [ "succ("; to_string e; ")" ]
    | Ifz (c, t, f) ->
        String.concat ~sep:""
          [ "ifz ("; to_string c; ") ("; to_string t; ") ("; to_string f; ")" ]
    | Lam (t, l, e) ->
        Printf.sprintf " (%s : %s). %s" l (to_string t) (to_string e)
    | App (e1, e2) -> Printf.sprintf "%s (%s)" (to_string e1) (to_string e2)
    | Fold (t, e) ->
        Printf.sprintf "fold{%s}(%s)" (Typ.TRec.to_string t) (to_string e)
    | Unfold e -> ""
    | Prod (a, b) -> ""
    | Unit _ -> "unit"
    | PrLeft e -> ""
    | PrRight e -> ""
    | InLeft (e, t) -> ""
    | InRight (t, e) -> ""
    | Case (e, f, s) -> ""
    | Abort (e, t) -> ""
end
