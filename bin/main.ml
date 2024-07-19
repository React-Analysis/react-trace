open! Core
open React_trace

let print_position (outx : Out_channel.t) (lexbuf : Lexing.lexbuf) : unit =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Out_channel.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error (lexbuf : Lexing.lexbuf) : Syntax.Prog.t =
  Parser.prog Lexer.read lexbuf

let get_program (filename : string) : Syntax.Prog.t =
  let filename, inx =
    if String.(filename = "-") then ("<stdin>", In_channel.stdin)
    else (filename, In_channel.create filename)
  in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

  match parse_with_error lexbuf with
  | prog ->
      In_channel.close inx;
      prog
  | exception Parser.Error ->
      Out_channel.fprintf stderr "%a: syntax error\n" print_position lexbuf;
      In_channel.close inx;
      Stdlib.exit 2

let () =
  let module Arg = Stdlib.Arg in
  let module Sys = Stdlib.Sys in
  let module Filename = Stdlib.Filename in
  let filename = ref "" in
  let opt_pp = ref false in
  let opt_parse_js = ref false in
  let opt_fuel = ref None in
  let opt_verbosity = ref Logs.Info in

  let usage_msg =
    "Usage : " ^ Filename.basename Sys.argv.(0) ^ " [-option] [filename] "
  in
  let speclist =
    [
      ("-pp", Arg.Unit (fun _ -> opt_pp := true), "Pretty-print program");
      ( "-parse-js",
        Arg.Unit (fun _ -> opt_parse_js := true),
        "Parse JS file with Flow" );
      ( "-verbose",
        Arg.Unit (fun _ -> opt_verbosity := Logs.Debug),
        "Verbose mode" );
      ("-fuel", Arg.Int (fun n -> opt_fuel := Some n), "[fuel] Run with fuel");
    ]
  in
  Arg.parse speclist (fun x -> filename := x) usage_msg;
  if String.is_empty !filename then Arg.usage speclist usage_msg
  else if !opt_parse_js then (
    let js_syntax, _ = Js_syntax.parse !filename in
    print_endline (Js_syntax.show js_syntax);
    let prog = Js_syntax.convert js_syntax in
    Sexp.pp_hum Stdlib.Format.std_formatter (Syntax.Prog.sexp_of_t prog))
  else (
    Fmt_tty.setup_std_outputs ();
    Logs.set_reporter (Logs_fmt.reporter ());
    Logs.set_level (Some !opt_verbosity);

    let prog = get_program !filename in

    if !opt_pp then
      Sexp.pp_hum Stdlib.Format.std_formatter (Syntax.Prog.sexp_of_t prog)
    else
      let { Interp.steps; _ } = Interp.run ?fuel:!opt_fuel prog in
      printf "\nSteps: %d\n" steps;
      Stdlib.exit (if Logs.err_count () > 0 then 1 else 0))
