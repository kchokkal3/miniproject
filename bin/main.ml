open Core
open Miniproject

let command =
  Command.basic ~summary:"Run Lambda or FPC files"
    (let%map_open.Command lang =
       flag "-l"
         (optional_with_default `Lambda
            (Arg_type.of_alist_exn [ ("lam", `Lambda); ("fpc", `FPC) ]))
         ~doc:"string Language to eval in"
     and filename =
       anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type))
     in
     fun () ->
       match lang with
       | `Lambda -> (
           let content = In_channel.read_all filename in
           let toks = Lexer.lex_lambda content in
           match toks with
           | Error e ->
               print_endline [%string "Lexing error: %{Error.to_string_hum e}"]
           | Ok toks -> (
               let progs = Parser.parse_lambda toks in
               match progs with
               | Error e ->
                   print_endline
                     [%string "Parsing error: %{Error.to_string_hum e}"]
               | Ok progs -> (
                   let evals = Interpreter.interpret_lambda progs in
                   match evals with
                   | Error e ->
                       print_endline
                         [%string "Evaluation error: %{Error.to_string_hum e}"]
                   | Ok evals ->
                       List.iteri evals ~f:(fun idx expr ->
                           print_endline
                             [%string
                               "Result %{Int.to_string idx}: \
                                %{Expr.LExpr.to_string expr}"]))))
       | `FPC -> ())

let () = Command_unix.run command
