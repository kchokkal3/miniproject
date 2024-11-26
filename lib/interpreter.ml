open! Core
open Expr
open Parser
open Or_error.Let_syntax

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

  let rec get_deep t string =
    let%bind expr = get t string in
    match expr with `Bound (LExpr.Var x) -> get_deep t x | e -> Ok e
end

let rec substitute_lambda (expr : LExpr.t) (name : string) (sub_expr : LExpr.t)
    =
  match expr with
  | Var x when String.equal x name -> sub_expr
  | Var x -> Var x
  | Lam (s, expr) when String.equal s name |> not ->
      Lam (s, substitute_lambda expr name sub_expr)
  | Lam (s, expr) -> Lam (s, expr)
  | App (fexpr, sexpr) ->
      App
        ( substitute_lambda fexpr name sub_expr,
          substitute_lambda sexpr name sub_expr )

let rec redex_lambda (expr : LExpr.t) (ctx : Context.t) =
  match expr with
  | Var x ->
      let%bind _ = Context.get ctx x in
      Ok (LExpr.Var x)
  | Lam (name, expr) ->
      let new_ctx = Context.add_defn ctx name in
      let%bind new_body = redex_lambda expr new_ctx in
      Ok (LExpr.Lam (name, new_body))
  | App (fexpr, sexpr) -> (
      let%bind new_fexpr = redex_lambda fexpr ctx in
      match new_fexpr with
      | Var x -> (
          let%bind eval_expr = Context.get_deep ctx x in
          match eval_expr with
          | `Bound (Lam (b, e)) ->
              let () = print_endline "HERE" in
              substitute_lambda e b sexpr |> Or_error.return
          | _ ->
              let%bind new_sexpr = redex_lambda sexpr ctx in
              Ok (LExpr.App (LExpr.Var x, new_sexpr)))
      | Lam (b, e) -> substitute_lambda e b sexpr |> Or_error.return
      | x ->
          let%bind new_sexpr = redex_lambda sexpr ctx in
          Ok (LExpr.App (x, new_sexpr)))

let rec can_redex_lambda (expr : LExpr.t) (ctx : Context.t) =
  let () = print_endline [%string "can_redex: %{LExpr.to_string expr}"] in
  match expr with
  | Lam (b, e) -> can_redex_lambda e (Context.add_defn ctx b)
  | App (f, s) -> (
      match can_redex_lambda f ctx with
      | true -> true
      | false -> (
          match f with
          | Lam (_, _) -> true
          | Var x -> (
              match Context.get_deep ctx x with
              | Ok (`Bound (Lam (_, _))) -> true
              | _ -> can_redex_lambda s ctx)
          | _ -> can_redex_lambda s ctx))
  | Var x -> (
      match Context.get_deep ctx x with
      | Ok `Defn -> false
      | Ok (`Bound _) -> false
      | Error _ -> false)

let rec interpret_expr_lambda (expr : LExpr.t) (ctx : Context.t) :
    LExpr.t Or_error.t =
  let () = LExpr.to_string expr |> print_endline in
  let () = LExpr.sexp_of_t expr |> print_s in
  let%bind new_expr = redex_lambda expr ctx in
  match can_redex_lambda new_expr ctx with
  | true -> interpret_expr_lambda new_expr ctx
  | false ->
      let () = LExpr.to_string new_expr |> print_endline in
      let () = LExpr.sexp_of_t new_expr |> print_s in
      let () = print_endline "++++++++++++" in
      Ok new_expr

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
          (* let expr = substitude_progs_lambda expr ctx in *)
          let%bind eval_expr = interpret_expr_lambda expr ctx in
          let new_ctx = Context.add_bound ctx name eval_expr in
          interpret_prog_lambda progs new_ctx
      | Eval expr ->
          (* let expr = substitude_progs_lambda expr ctx in *)
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
                    LExpr.sexp_of_t expr |> print_sexp ~tag:"Eval Result")))
  in
  let _ = interpret_and_print "defn I = \\x. x\neval I I" in
  ();
  [%expect
    {|
    \x. x
    (Lam x (Var x))
    can_redex: \x. x
    can_redex: x
    \x. x
    (Lam x (Var x))
    ++++++++++++
    (I) (I)
    (App (Var I) (Var I))
    HERE
    can_redex: I
    I
    (Var I)
    ++++++++++++
    Eval Result:
     (Var I)
    -----------
    |}];
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
    \s. \z. z
    (Lam s (Lam z (Var z)))
    can_redex: \s. \z. z
    can_redex: \z. z
    can_redex: z
    \s. \z. z
    (Lam s (Lam z (Var z)))
    ++++++++++++
    \n. \s. \z. (s) (((n) (s)) (z))
    (Lam n (Lam s (Lam z (App (Var s) (App (App (Var n) (Var s)) (Var z))))))
    can_redex: \n. \s. \z. (s) (((n) (s)) (z))
    can_redex: \s. \z. (s) (((n) (s)) (z))
    can_redex: \z. (s) (((n) (s)) (z))
    can_redex: (s) (((n) (s)) (z))
    can_redex: s
    can_redex: ((n) (s)) (z)
    can_redex: (n) (s)
    can_redex: n
    can_redex: s
    can_redex: z
    \n. \s. \z. (s) (((n) (s)) (z))
    (Lam n (Lam s (Lam z (App (Var s) (App (App (Var n) (Var s)) (Var z))))))
    ++++++++++++
    (succ) (zero)
    (App (Var succ) (Var zero))
    HERE
    can_redex: \s. \z. (s) (((zero) (s)) (z))
    can_redex: \z. (s) (((zero) (s)) (z))
    can_redex: (s) (((zero) (s)) (z))
    can_redex: s
    can_redex: ((zero) (s)) (z)
    can_redex: (zero) (s)
    can_redex: zero
    \s. \z. (s) (((zero) (s)) (z))
    (Lam s (Lam z (App (Var s) (App (App (Var zero) (Var s)) (Var z)))))
    HERE
    can_redex: \s. \z. (s) (z)
    can_redex: \z. (s) (z)
    can_redex: (s) (z)
    can_redex: s
    can_redex: z
    \s. \z. (s) (z)
    (Lam s (Lam z (App (Var s) (Var z))))
    ++++++++++++
    (succ) (one)
    (App (Var succ) (Var one))
    HERE
    can_redex: \s. \z. (s) (((one) (s)) (z))
    can_redex: \z. (s) (((one) (s)) (z))
    can_redex: (s) (((one) (s)) (z))
    can_redex: s
    can_redex: ((one) (s)) (z)
    can_redex: (one) (s)
    can_redex: one
    \s. \z. (s) (((one) (s)) (z))
    (Lam s (Lam z (App (Var s) (App (App (Var one) (Var s)) (Var z)))))
    HERE
    can_redex: \s. \z. (s) ((s) (z))
    can_redex: \z. (s) ((s) (z))
    can_redex: (s) ((s) (z))
    can_redex: s
    can_redex: (s) (z)
    can_redex: s
    can_redex: z
    \s. \z. (s) ((s) (z))
    (Lam s (Lam z (App (Var s) (App (Var s) (Var z)))))
    ++++++++++++
    \n. \k. ((n) (succ)) (k)
    (Lam n (Lam k (App (App (Var n) (Var succ)) (Var k))))
    can_redex: \n. \k. ((n) (succ)) (k)
    can_redex: \k. ((n) (succ)) (k)
    can_redex: ((n) (succ)) (k)
    can_redex: (n) (succ)
    can_redex: n
    can_redex: succ
    can_redex: k
    \n. \k. ((n) (succ)) (k)
    (Lam n (Lam k (App (App (Var n) (Var succ)) (Var k))))
    ++++++++++++
    \n. \k. ((n) ((plus) (k))) (zero)
    (Lam n (Lam k (App (App (Var n) (App (Var plus) (Var k))) (Var zero))))
    HERE
    can_redex: \n. \k. ((n) (\k. ((k) (succ)) (k))) (zero)
    can_redex: \k. ((n) (\k. ((k) (succ)) (k))) (zero)
    can_redex: ((n) (\k. ((k) (succ)) (k))) (zero)
    can_redex: (n) (\k. ((k) (succ)) (k))
    can_redex: n
    can_redex: \k. ((k) (succ)) (k)
    can_redex: ((k) (succ)) (k)
    can_redex: (k) (succ)
    can_redex: k
    can_redex: succ
    can_redex: k
    can_redex: zero
    \n. \k. ((n) (\k. ((k) (succ)) (k))) (zero)
    (Lam n
     (Lam k
      (App (App (Var n) (Lam k (App (App (Var k) (Var succ)) (Var k))))
       (Var zero))))
    ++++++++++++
    \n. \k. ((k) ((times) (n))) (one)
    (Lam n (Lam k (App (App (Var k) (App (Var times) (Var n))) (Var one))))
    HERE
    can_redex: \n. \k. ((k) (\k. ((n) (\k. ((k) (succ)) (k))) (zero))) (one)
    can_redex: \k. ((k) (\k. ((n) (\k. ((k) (succ)) (k))) (zero))) (one)
    can_redex: ((k) (\k. ((n) (\k. ((k) (succ)) (k))) (zero))) (one)
    can_redex: (k) (\k. ((n) (\k. ((k) (succ)) (k))) (zero))
    can_redex: k
    can_redex: \k. ((n) (\k. ((k) (succ)) (k))) (zero)
    can_redex: ((n) (\k. ((k) (succ)) (k))) (zero)
    can_redex: (n) (\k. ((k) (succ)) (k))
    can_redex: n
    can_redex: \k. ((k) (succ)) (k)
    can_redex: ((k) (succ)) (k)
    can_redex: (k) (succ)
    can_redex: k
    can_redex: succ
    can_redex: k
    can_redex: zero
    can_redex: one
    \n. \k. ((k) (\k. ((n) (\k. ((k) (succ)) (k))) (zero))) (one)
    (Lam n
     (Lam k
      (App
       (App (Var k)
        (Lam k
         (App (App (Var n) (Lam k (App (App (Var k) (Var succ)) (Var k))))
          (Var zero))))
       (Var one))))
    ++++++++++++
    (succ) (zero)
    (App (Var succ) (Var zero))
    HERE
    can_redex: \s. \z. (s) (((zero) (s)) (z))
    can_redex: \z. (s) (((zero) (s)) (z))
    can_redex: (s) (((zero) (s)) (z))
    can_redex: s
    can_redex: ((zero) (s)) (z)
    can_redex: (zero) (s)
    can_redex: zero
    \s. \z. (s) (((zero) (s)) (z))
    (Lam s (Lam z (App (Var s) (App (App (Var zero) (Var s)) (Var z)))))
    HERE
    can_redex: \s. \z. (s) (z)
    can_redex: \z. (s) (z)
    can_redex: (s) (z)
    can_redex: s
    can_redex: z
    \s. \z. (s) (z)
    (Lam s (Lam z (App (Var s) (Var z))))
    ++++++++++++
    (succ) (one)
    (App (Var succ) (Var one))
    HERE
    can_redex: \s. \z. (s) (((one) (s)) (z))
    can_redex: \z. (s) (((one) (s)) (z))
    can_redex: (s) (((one) (s)) (z))
    can_redex: s
    can_redex: ((one) (s)) (z)
    can_redex: (one) (s)
    can_redex: one
    \s. \z. (s) (((one) (s)) (z))
    (Lam s (Lam z (App (Var s) (App (App (Var one) (Var s)) (Var z)))))
    HERE
    can_redex: \s. \z. (s) ((s) (z))
    can_redex: \z. (s) ((s) (z))
    can_redex: (s) ((s) (z))
    can_redex: s
    can_redex: (s) (z)
    can_redex: s
    can_redex: z
    \s. \z. (s) ((s) (z))
    (Lam s (Lam z (App (Var s) (App (Var s) (Var z)))))
    ++++++++++++
    ((plus) (one)) (one)
    (App (App (Var plus) (Var one)) (Var one))
    HERE
    can_redex: ((one) (succ)) (one)
    can_redex: (one) (succ)
    can_redex: one
    ((one) (succ)) (one)
    (App (App (Var one) (Var succ)) (Var one))
    HERE
    can_redex: (succ) (one)
    can_redex: succ
    (succ) (one)
    (App (Var succ) (Var one))
    HERE
    can_redex: \s. \z. (s) (((one) (s)) (z))
    can_redex: \z. (s) (((one) (s)) (z))
    can_redex: (s) (((one) (s)) (z))
    can_redex: s
    can_redex: ((one) (s)) (z)
    can_redex: (one) (s)
    can_redex: one
    \s. \z. (s) (((one) (s)) (z))
    (Lam s (Lam z (App (Var s) (App (App (Var one) (Var s)) (Var z)))))
    HERE
    can_redex: \s. \z. (s) ((s) (z))
    can_redex: \z. (s) ((s) (z))
    can_redex: (s) ((s) (z))
    can_redex: s
    can_redex: (s) (z)
    can_redex: s
    can_redex: z
    \s. \z. (s) ((s) (z))
    (Lam s (Lam z (App (Var s) (App (Var s) (Var z)))))
    ++++++++++++
    ((times) (one)) (zero)
    (App (App (Var times) (Var one)) (Var zero))
    HERE
    can_redex: ((one) (\k. ((k) (succ)) (k))) (zero)
    can_redex: (one) (\k. ((k) (succ)) (k))
    can_redex: one
    ((one) (\k. ((k) (succ)) (k))) (zero)
    (App (App (Var one) (Lam k (App (App (Var k) (Var succ)) (Var k))))
     (Var zero))
    HERE
    can_redex: (\k. ((k) (succ)) (k)) (zero)
    can_redex: \k. ((k) (succ)) (k)
    can_redex: ((k) (succ)) (k)
    can_redex: (k) (succ)
    can_redex: k
    can_redex: succ
    can_redex: k
    (\k. ((k) (succ)) (k)) (zero)
    (App (Lam k (App (App (Var k) (Var succ)) (Var k))) (Var zero))
    can_redex: ((zero) (succ)) (zero)
    can_redex: (zero) (succ)
    can_redex: zero
    ((zero) (succ)) (zero)
    (App (App (Var zero) (Var succ)) (Var zero))
    HERE
    can_redex: zero
    zero
    (Var zero)
    ++++++++++++
    ((times) (two)) (two)
    (App (App (Var times) (Var two)) (Var two))
    HERE
    can_redex: ((two) (\k. ((k) (succ)) (k))) (zero)
    can_redex: (two) (\k. ((k) (succ)) (k))
    can_redex: two
    ((two) (\k. ((k) (succ)) (k))) (zero)
    (App (App (Var two) (Lam k (App (App (Var k) (Var succ)) (Var k))))
     (Var zero))
    HERE
    can_redex: (\k. ((k) (succ)) (k)) ((\k. ((k) (succ)) (k)) (zero))
    can_redex: \k. ((k) (succ)) (k)
    can_redex: ((k) (succ)) (k)
    can_redex: (k) (succ)
    can_redex: k
    can_redex: succ
    can_redex: k
    (\k. ((k) (succ)) (k)) ((\k. ((k) (succ)) (k)) (zero))
    (App (Lam k (App (App (Var k) (Var succ)) (Var k)))
     (App (Lam k (App (App (Var k) (Var succ)) (Var k))) (Var zero)))
    can_redex: (((\k. ((k) (succ)) (k)) (zero)) (succ)) ((\k. ((k) (succ)) (k)) (zero))
    can_redex: ((\k. ((k) (succ)) (k)) (zero)) (succ)
    can_redex: (\k. ((k) (succ)) (k)) (zero)
    can_redex: \k. ((k) (succ)) (k)
    can_redex: ((k) (succ)) (k)
    can_redex: (k) (succ)
    can_redex: k
    can_redex: succ
    can_redex: k
    (((\k. ((k) (succ)) (k)) (zero)) (succ)) ((\k. ((k) (succ)) (k)) (zero))
    (App
     (App (App (Lam k (App (App (Var k) (Var succ)) (Var k))) (Var zero))
      (Var succ))
     (App (Lam k (App (App (Var k) (Var succ)) (Var k))) (Var zero)))
    can_redex: ((((zero) (succ)) (zero)) (succ)) (((zero) (succ)) (zero))
    can_redex: (((zero) (succ)) (zero)) (succ)
    can_redex: ((zero) (succ)) (zero)
    can_redex: (zero) (succ)
    can_redex: zero
    ((((zero) (succ)) (zero)) (succ)) (((zero) (succ)) (zero))
    (App (App (App (App (Var zero) (Var succ)) (Var zero)) (Var succ))
     (App (App (Var zero) (Var succ)) (Var zero)))
    HERE
    HERE
    can_redex: ((zero) (succ)) (zero)
    can_redex: (zero) (succ)
    can_redex: zero
    ((zero) (succ)) (zero)
    (App (App (Var zero) (Var succ)) (Var zero))
    HERE
    can_redex: zero
    zero
    (Var zero)
    ++++++++++++
    ((exp) (two)) (one)
    (App (App (Var exp) (Var two)) (Var one))
    HERE
    can_redex: ((one) (\k. ((two) (\k. ((k) (succ)) (k))) (zero))) (one)
    can_redex: (one) (\k. ((two) (\k. ((k) (succ)) (k))) (zero))
    can_redex: one
    ((one) (\k. ((two) (\k. ((k) (succ)) (k))) (zero))) (one)
    (App
     (App (Var one)
      (Lam k
       (App (App (Var two) (Lam k (App (App (Var k) (Var succ)) (Var k))))
        (Var zero))))
     (Var one))
    HERE
    can_redex: (\k. ((two) (\k. ((k) (succ)) (k))) (zero)) (one)
    can_redex: \k. ((two) (\k. ((k) (succ)) (k))) (zero)
    can_redex: ((two) (\k. ((k) (succ)) (k))) (zero)
    can_redex: (two) (\k. ((k) (succ)) (k))
    can_redex: two
    (\k. ((two) (\k. ((k) (succ)) (k))) (zero)) (one)
    (App
     (Lam k
      (App (App (Var two) (Lam k (App (App (Var k) (Var succ)) (Var k))))
       (Var zero)))
     (Var one))
    HERE
    can_redex: (\k. ((k) (succ)) (k)) ((\k. ((k) (succ)) (k)) (zero))
    can_redex: \k. ((k) (succ)) (k)
    can_redex: ((k) (succ)) (k)
    can_redex: (k) (succ)
    can_redex: k
    can_redex: succ
    can_redex: k
    (\k. ((k) (succ)) (k)) ((\k. ((k) (succ)) (k)) (zero))
    (App (Lam k (App (App (Var k) (Var succ)) (Var k)))
     (App (Lam k (App (App (Var k) (Var succ)) (Var k))) (Var zero)))
    can_redex: (((\k. ((k) (succ)) (k)) (zero)) (succ)) ((\k. ((k) (succ)) (k)) (zero))
    can_redex: ((\k. ((k) (succ)) (k)) (zero)) (succ)
    can_redex: (\k. ((k) (succ)) (k)) (zero)
    can_redex: \k. ((k) (succ)) (k)
    can_redex: ((k) (succ)) (k)
    can_redex: (k) (succ)
    can_redex: k
    can_redex: succ
    can_redex: k
    (((\k. ((k) (succ)) (k)) (zero)) (succ)) ((\k. ((k) (succ)) (k)) (zero))
    (App
     (App (App (Lam k (App (App (Var k) (Var succ)) (Var k))) (Var zero))
      (Var succ))
     (App (Lam k (App (App (Var k) (Var succ)) (Var k))) (Var zero)))
    can_redex: ((((zero) (succ)) (zero)) (succ)) (((zero) (succ)) (zero))
    can_redex: (((zero) (succ)) (zero)) (succ)
    can_redex: ((zero) (succ)) (zero)
    can_redex: (zero) (succ)
    can_redex: zero
    ((((zero) (succ)) (zero)) (succ)) (((zero) (succ)) (zero))
    (App (App (App (App (Var zero) (Var succ)) (Var zero)) (Var succ))
     (App (App (Var zero) (Var succ)) (Var zero)))
    HERE
    HERE
    can_redex: ((zero) (succ)) (zero)
    can_redex: (zero) (succ)
    can_redex: zero
    ((zero) (succ)) (zero)
    (App (App (Var zero) (Var succ)) (Var zero))
    HERE
    can_redex: zero
    zero
    (Var zero)
    ++++++++++++
    Eval Result:
     (Lam s (Lam z (App (Var s) (Var z))))
    -----------
    Eval Result:
     (Lam s (Lam z (App (Var s) (App (Var s) (Var z)))))
    -----------
    Eval Result:
     (Lam s (Lam z (App (Var s) (App (Var s) (Var z)))))
    -----------
    Eval Result:
     (Var zero)
    -----------
    Eval Result:
     (Var zero)
    -----------
    Eval Result:
     (Var zero)
    -----------
    |}]
