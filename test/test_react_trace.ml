open! Base
open React_trace

let fuel = 100

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let set_in_body_nonterminate () =
  let prog =
    parse
      {|
let C x =
  stt s, setS = 42 in
  setS (fun s -> 43);
  view [()]
;;
view [C ()]
|}
  in
  let run () =
    Interp.(re_render_limit_h (run ~fuel) prog ~re_render_limit:25) |> ignore
  in
  Alcotest.(check_raises) "retry indefintely" Interp.Too_many_re_renders run

let set_in_effect_step_three_times () =
  let prog =
    parse
      {|
let C x =
  stt s, setS = 42 in
  eff (setS (fun s -> 43));
  view [()]
;;
view [C ()]
|}
  in
  let { Interp.steps } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step three times" ~expected:3 ~actual:steps

let set_in_effect_step_indefinitely () =
  let prog =
    parse
      {|
let C x =
  stt s, setS = 42 in
  eff (setS (fun s -> s + 1));
  view [()]
;;
view [C ()]
|}
  in
  let { Interp.steps } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step indefintely" ~expected:fuel ~actual:steps

let () =
  let open Alcotest in
  run "Interpreter"
    [
      ( "steps",
        [
          test_case "Set in body should not terminate" `Quick
            set_in_body_nonterminate;
          test_case "Set in effect should step three times" `Quick
            set_in_effect_step_three_times;
          test_case "Set in effect should step indefintely" `Quick
            set_in_effect_step_indefinitely;
        ] );
    ]
