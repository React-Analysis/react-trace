open! Base
open React_trace

let fuel = 100

let parse_prog s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let parse_expr s =
  let lexbuf = Lexing.from_string s in
  Parser.expr Lexer.read lexbuf

let parse_unit () =
  let open Syntax in
  let (Ex expr) = parse_expr "()" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse unit" ~actual:(Expr.sexp_of_t expr)
    ~expected:Expr.(sexp_of_t (Const Unit))

let parse_true () =
  let open Syntax in
  let (Ex expr) = parse_expr "true" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse true" ~actual:(Expr.sexp_of_t expr)
    ~expected:Expr.(sexp_of_t (Const (Bool true)))

let parse_false () =
  let open Syntax in
  let (Ex expr) = parse_expr "false" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse false" ~actual:(Expr.sexp_of_t expr)
    ~expected:Expr.(sexp_of_t (Const (Bool false)))

let parse_int () =
  let open Syntax in
  let (Ex expr) = parse_expr "42" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse int" ~actual:(Expr.sexp_of_t expr)
    ~expected:Expr.(sexp_of_t (Const (Int 42)))

let parse_var () =
  let open Syntax in
  let (Ex expr) = parse_expr "_some_variable123" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse var" ~actual:(Expr.sexp_of_t expr)
    ~expected:Expr.(sexp_of_t (Var "_some_variable123"))

let parse_view () =
  let open Syntax in
  let (Ex expr) = parse_expr "view [(), 42, (), Comp ()]" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse view" ~actual:(Expr.sexp_of_t expr)
    ~expected:
      Expr.(
        sexp_of_t
          (View
             [
               Const Unit;
               Const (Int 42);
               Const Unit;
               App { fn = Var "Comp"; arg = Const Unit };
             ]))

let parse_open_cond () =
  let open Syntax in
  let (Ex expr) = parse_expr "if true then if true then ()" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse open cond" ~expected:(Expr.sexp_of_t expr)
    ~actual:
      Expr.(
        sexp_of_t
          (Cond
             {
               pred = Const (Bool true);
               con =
                 Cond
                   {
                     pred = Const (Bool true);
                     con = Const Unit;
                     alt = Const Unit;
                   };
               alt = Const Unit;
             }))

let parse_closed_cond () =
  let open Syntax in
  let (Ex expr) = parse_expr "if true then if true then () else ()" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse closed cond" ~expected:(Expr.sexp_of_t expr)
    ~actual:
      Expr.(
        sexp_of_t
          (Cond
             {
               pred = Const (Bool true);
               con =
                 Cond
                   {
                     pred = Const (Bool true);
                     con = Const Unit;
                     alt = Const Unit;
                   };
               alt = Const Unit;
             }))

let parse_fn () =
  let open Syntax in
  let (Ex expr) = parse_expr "fun x -> fun y -> x + y" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse closed cond" ~expected:(Expr.sexp_of_t expr)
    ~actual:
      Expr.(
        sexp_of_t
          (Fn
             {
               param = "x";
               body =
                 Fn
                   {
                     param = "y";
                     body = Bop { op = Plus; left = Var "x"; right = Var "y" };
                   };
             }))

let parse_app () =
  let open Syntax in
  let (Ex expr) = parse_expr "a b c" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse app" ~actual:(Expr.sexp_of_t expr)
    ~expected:
      Expr.(
        sexp_of_t
          (App { fn = App { fn = Var "a"; arg = Var "b" }; arg = Var "c" }))

let parse_let () =
  let open Syntax in
  let (Ex expr) = parse_expr "let x = let y = 1 in y in let z = x in z" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse let" ~actual:(Expr.sexp_of_t expr)
    ~expected:
      Expr.(
        sexp_of_t
          (Let
             {
               id = "x";
               bound = Let { id = "y"; bound = Const (Int 1); body = Var "y" };
               body = Let { id = "z"; bound = Var "x"; body = Var "z" };
             }))

let parse_stt () =
  let open Syntax in
  let (Ex expr) = parse_expr "stt x, setX = 42 in stt y, setY = -42 in x + y" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse stt" ~actual:(Expr.sexp_of_t expr)
    ~expected:
      Expr.(
        sexp_of_t
          (Stt
             {
               label = 0;
               stt = "x";
               set = "setX";
               init = Const (Int 42);
               body =
                 Stt
                   {
                     label = 1;
                     stt = "y";
                     set = "setY";
                     init = Uop { op = Uminus; arg = Const (Int 42) };
                     body = Bop { op = Plus; left = Var "x"; right = Var "y" };
                   };
             }))

let parse_eff () =
  let open Syntax in
  let (Ex expr) = parse_expr "eff (x ())" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse eff" ~actual:(Expr.sexp_of_t expr)
    ~expected:Expr.(sexp_of_t (Eff (App { fn = Var "x"; arg = Const Unit })))

let parse_seq () =
  let open Syntax in
  let (Ex expr) = parse_expr "a; b; c; d" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse seq" ~actual:(Expr.sexp_of_t expr)
    ~expected:
      Expr.(sexp_of_t (Seq (Var "a", Seq (Var "b", Seq (Var "c", Var "d")))))

let parse_op () =
  let open Syntax in
  let (Ex expr) = parse_expr "not (+-42 <= 0 + -0) || true" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse op" ~actual:(Expr.sexp_of_t expr)
    ~expected:
      Expr.(
        sexp_of_t
          (Bop
             {
               op = Or;
               left =
                 Uop
                   {
                     op = Not;
                     arg =
                       Bop
                         {
                           op = Le;
                           left =
                             Uop
                               {
                                 op = Uplus;
                                 arg = Uop { op = Uminus; arg = Const (Int 42) };
                               };
                           right =
                             Bop
                               {
                                 op = Plus;
                                 left = Const (Int 0);
                                 right =
                                   Uop { op = Uminus; arg = Const (Int 0) };
                               };
                         };
                   };
               right = Const (Bool true);
             }))

let no_side_effect () =
  let prog =
    parse_prog {|
let C x =
  stt s, setS = 42 in
  view [()]
;;
view [C ()]
|}
  in
  let { Interp.steps } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step two times" ~expected:2 ~actual:steps

let set_in_body_nonterminate () =
  let prog =
    parse_prog
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
    parse_prog
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
    parse_prog
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
      ( "parse",
        [
          test_case "unit" `Quick parse_unit;
          test_case "true" `Quick parse_true;
          test_case "false" `Quick parse_false;
          test_case "int" `Quick parse_int;
          test_case "var" `Quick parse_var;
          test_case "view" `Quick parse_view;
          test_case "open cond" `Quick parse_open_cond;
          test_case "closed cond" `Quick parse_closed_cond;
          test_case "fn" `Quick parse_fn;
          test_case "app" `Quick parse_app;
          test_case "let" `Quick parse_let;
          test_case "stt" `Quick parse_stt;
          test_case "eff" `Quick parse_eff;
          test_case "seq" `Quick parse_seq;
          test_case "op" `Quick parse_op;
        ] );
      ( "steps",
        [
          test_case "No side effect should step two times" `Quick no_side_effect;
          test_case "Set in body should not terminate" `Quick
            set_in_body_nonterminate;
          test_case "Set in effect should step three times" `Quick
            set_in_effect_step_three_times;
          test_case "Set in effect should step indefintely" `Quick
            set_in_effect_step_indefinitely;
        ] );
    ]
