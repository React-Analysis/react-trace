open! Core
open React_trace

let position (lexbuf : Lexing.lexbuf) : string =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error (lexbuf : Lexing.lexbuf) : Syntax.Prog.t =
  Parser.prog Lexer.read lexbuf

let parse_program_str (program_str : string) : (Syntax.Prog.t, string) result =
  let lexbuf = Lexing.from_string program_str in
  match parse_with_error lexbuf with
  | prog -> Ok prog
  | exception Parser.Error ->
      Error (sprintf "%s: syntax error" (position lexbuf))

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);

  let open Js_of_ocaml in
  Js.export_all
    (object%js
       method run (fuel : int) program_str =
         (let open Result.Let_syntax in
          let%bind prog = parse_program_str program_str in
          let Interp.{ recording; _ } =
            Interp.run
              ?fuel:(if fuel < 1 then None else Some fuel)
              ~recorder:(module Recorder)
              prog
          in
          if Logs.err_count () > 0 then Error "error" else Ok recording)
         |> (function Ok s -> s | Error s -> s)
         |> Js.string
    end)
