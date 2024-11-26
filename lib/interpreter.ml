open! Core
open Expr
open Parser
open Or_error.Let_syntax

let fresh = ref 0

module Context = struct
  type t = (string * [ `Defn | `Bound of LExpr.t ]) list [@@deriving sexp]

  let add_defn (t : t) string = (string, `Defn) :: t
  let add_bound (t : t) string expr = (string, `Bound expr) :: t

  let get_bound_exprs (t : t) =
    List.filter_map t ~f:(fun (name, expr) ->
        match expr with `Defn -> None | `Bound e -> Some (name, e))

  let rec get t string =
    match t with
    | [] -> Or_error.error_string [%string "Unknown name %{string}"]
    | (s, expr) :: ts -> (
        match String.equal s string with
        | true -> Ok expr
        | false -> get ts string)
end

let rec substitute_lambda (expr : LExpr.t) (name : string) (sub_expr : LExpr.t)
    =
  match expr with
  | Var x when String.equal x name -> sub_expr
  | Var x -> Var x
  | Lam (s, expr) when String.equal s name -> Lam (s, expr)
  | Lam (s, expr) -> (
      match sub_expr with
      | Var b when String.equal b s ->
          let id = !fresh in
          fresh := !fresh + 1;
          let new_s = [%string "%{s}%{Int.to_string id}"] in
          let new_expr = substitute_lambda expr s (LExpr.Var new_s) in
          Lam (new_s, substitute_lambda new_expr name sub_expr)
      | _ -> Lam (s, substitute_lambda expr name sub_expr))
  | App (fexpr, sexpr) ->
      App
        ( substitute_lambda fexpr name sub_expr,
          substitute_lambda sexpr name sub_expr )

let rec redex_lambda (expr : LExpr.t) (ctx : Context.t) =
  match expr with
  | Var x -> (
      let%bind var = Context.get ctx x in
      match var with `Defn -> Ok (LExpr.Var x) | `Bound e -> Ok e)
  | Lam (name, expr) ->
      let new_ctx = Context.add_defn ctx name in
      let%bind new_body = redex_lambda expr new_ctx in
      Ok (LExpr.Lam (name, new_body))
  | App (fexpr, sexpr) -> (
      match fexpr with
      | Lam (b, e) -> substitute_lambda e b sexpr |> Or_error.return
      | _ ->
          let%bind new_fexpr = redex_lambda fexpr ctx in
          let%bind new_sexpr = redex_lambda sexpr ctx in
          Ok (LExpr.App (new_fexpr, new_sexpr)))

let rec can_redex_lambda (expr : LExpr.t) (ctx : Context.t) =
  match expr with
  | Lam (b, e) -> can_redex_lambda e (Context.add_defn ctx b)
  | App (f, s) -> (
      match f with
      | Lam (_, _) -> true
      | _ -> can_redex_lambda f ctx || can_redex_lambda s ctx)
  | Var x -> (
      match Context.get ctx x with
      | Ok `Defn -> false
      | Ok (`Bound _) -> true
      | Error _ -> false)

let rec interpret_expr_lambda (expr : LExpr.t) (ctx : Context.t) :
    LExpr.t Or_error.t =
  let%bind new_expr = redex_lambda expr ctx in
  match can_redex_lambda new_expr ctx with
  | true -> interpret_expr_lambda new_expr ctx
  | false -> Ok new_expr

let _substitute_progs_lambda (expr : LExpr.t) (ctx : Context.t) =
  List.fold ~init:expr
    ~f:(fun expr (name, bound_expr) -> substitute_lambda expr name bound_expr)
    (Context.get_bound_exprs ctx)

let rec interpret_prog_lambda (progs : LProg.t list) (ctx : Context.t) :
    LExpr.t list Or_error.t =
  match progs with
  | [] -> Ok []
  | prog :: progs -> (
      match prog with
      | Defn (name, expr) ->
          (* let%bind eval_expr = interpret_expr_lambda expr ctx in *)
          let new_ctx = Context.add_bound ctx name expr in
          interpret_prog_lambda progs new_ctx
      | Eval expr ->
          let%bind eval_expr = interpret_expr_lambda expr ctx in
          let rest_err = interpret_prog_lambda progs ctx in
          let%bind rest = rest_err in
          Ok (eval_expr :: rest))

let interpret_lambda progs = interpret_prog_lambda progs []

let%expect_test _ =
  let print_sexp sexp ~tag =
    print_endline [%string "%{tag}:\n %{Sexp.to_string_hum sexp}\n-----------"]
  in
  let interpret_and_print s =
    let toks = Lexer.lex_lambda s in
    match toks with
    | Error e -> Error.sexp_of_t e |> print_sexp ~tag:"Error in lexing"
    | Ok toks -> (
        let progs = parse_lambda toks in
        match progs with
        | Error e -> Error.sexp_of_t e |> print_sexp ~tag:"Error in parsing"
        | Ok progs -> (
            let exprs = interpret_lambda progs in
            match exprs with
            | Error e ->
                Error.sexp_of_t e |> print_sexp ~tag:"Error in evaluating"
            | Ok exprs ->
                List.iter exprs ~f:(fun expr ->
                    print_endline
                      [%string "Eval result: %{LExpr.to_string expr}"])))
  in
  let _ = interpret_and_print "defn I = \\x. x\neval I I" in
  ();
  [%expect {| Eval result: \x. x |}];
  let _ =
    interpret_and_print
      {|defn zero = \s. \z. z
     defn succ = \n. \s. \z. s (n s z)
defn one = succ zero
defn two = succ one
defn plus = \n. \k. n succ k
defn times = \n. \k. n (plus k) zero
defn exp = \n. \k. k (times n) one
eval succ zero
eval succ one
eval plus one one
eval times one zero
eval times two two
eval exp two one
   |}
  in
  ();
  [%expect
    {|
    Eval result: \s. \z. (s) (z)
    Eval result: \s. \z. (s) ((s) (z))
    Eval result: \s. \z. (s) ((s) (z))
    Eval result: \s. \z. z
    Eval result: \s. \z. (s) ((s) ((s) ((s) (z))))
    Eval result: \s. \z. (s) ((s) (z))
            |}];
  let _ =
    interpret_and_print
      {|
defn zero = \s. \z. z
defn succ = \n. \s. \z. s (n s z)
defn one = succ zero
defn two = succ one
defn three = succ two
defn plus = \n. \k. n succ k
defn times = \n. \k. n (plus k) zero
defn exp = \n. \k. k (times n) one
defn pair = \x. \y. \k. k x y
defn pred2 = \n. n (\p. p (\x. \y. pair (succ x) x)) (pair zero zero)
defn pred = \n. pred2 n (\x. \y. y)
defn Y = \h. (\x. h (x x)) (\x. h (x x))
defn if0 = \n. \x. \y. n (\a. y) x
defn if1 = \n. \x. \y. if0 n y (if0 (one pred n) x y)
% Inspiration for how to structure general recursive functions from lecture notes
defn h_lucas = \f. \n. if0 n two (if1 n one (plus (f (two pred n)) (f (one pred n))))
defn lucas = Y h_lucas

eval lucas zero
eval lucas one
eval lucas two
eval lucas (times two two)
   |}
  in
  ();
  [%expect
    {|
    Eval result: \s. \z. (s) ((s) (z))
    Eval result: \s. \z. (s) (z)
    Eval result: \s. \z. (s) ((s) ((s) (z)))
    Eval result: \s. \z. (s) ((s) ((s) ((s) (z))))
    |}]
