open! Base
open React_trace
open Lib_domains

let position (lexbuf : Lexing.lexbuf) : string =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error (lexbuf : Lexing.lexbuf) : Syntax.Prog.t =
  Parser.prog Lexer.read lexbuf

let parse_program_str (program_str : string) : (Syntax.Prog.t, string) Result.t
    =
  let lexbuf = Lexing.from_string program_str in
  match parse_with_error lexbuf with
  | prog -> Ok prog
  | exception Parser.Error ->
      Error (Printf.sprintf "%s: syntax error" (position lexbuf))

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Error);

  let open Js_of_ocaml in
  Js.export_all
    (object%js
       method run (fuel : int) program_str =
         (let ( let* ) x f = Result.bind x ~f in
          let* prog = parse_program_str program_str in
          let Interp.{ recording; _ } =
            Interp.run
              ?fuel:(if fuel < 1 then None else Some fuel)
              ~recorder:(module Recorder)
              prog
          in
          if Logs.err_count () > 0 then Error "error" else Ok recording)
         |> ( function
         | Ok s ->
             [|
               ("log", s.log |> Js.string |> Js.Unsafe.inject);
               ( "checkpoints",
                 s.checkpoints
                 |> Array.of_list_rev_map ~f:(fun { Recorder.msg } ->
                        Js.string msg)
                 |> Js.array |> Js.Unsafe.inject );
             |]
         | Error s -> [| ("error", s |> Js.string |> Js.Unsafe.inject) |] )
         |> Js.Unsafe.obj
    end)
