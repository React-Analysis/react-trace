open! Core
open Stdlib.Effect.Deep
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
  let (Ex expr) = parse_expr "not (+-a () <= 0 + -0) || true" in
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
                                 arg =
                                   Uop
                                     {
                                       op = Uminus;
                                       arg =
                                         App { fn = Var "a"; arg = Const Unit };
                                     };
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

let parse_obj () =
  let open Syntax in
  let (Ex expr) = parse_expr "let x = {} in x.y := 3; x.y" in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse obj" ~actual:(Expr.sexp_of_t expr)
    ~expected:
      Expr.(
        sexp_of_t
          (Let
             {
               id = "x";
               bound = Alloc;
               body =
                 Seq
                   ( Set { obj = Var "x"; field = "y"; value = Const (Int 3) },
                     Get { obj = Var "x"; field = "y" } );
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
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step one time" ~expected:1 ~actual:steps

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
    Interp.(match_with (run ~fuel) prog re_render_limit_h ~re_render_limit:25)
    |> ignore
  in
  Alcotest.(check_raises) "retry indefintely" Interp.Too_many_re_renders run

let set_in_body_guarded () =
  let prog =
    parse_prog
      {|
let C x =
  stt s, setS = 42 in
  if s = 42 then setS (fun s -> 43);
  view [()]
;;
view [C ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step one time" ~expected:1 ~actual:steps

let set_in_effect_step_one_time () =
  let prog =
    parse_prog
      {|
let C x =
  stt s, setS = 42 in
  eff (setS (fun s -> 42));
  view [()]
;;
view [C ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step two times" ~expected:1 ~actual:steps

let set_in_effect_step_two_times () =
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
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step two times" ~expected:2 ~actual:steps

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
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step indefintely" ~expected:fuel ~actual:steps

let set_in_effect_guarded_step_two_times () =
  let prog =
    parse_prog
      {|
let C x =
  stt s, setS = 42 in
  eff (if s = 42 then setS (fun s -> 43));
  view [()]
;;
view [C ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step two times" ~expected:2 ~actual:steps

let set_in_effect_guarded_step_n_times () =
  let prog =
    parse_prog
      {|
let C x =
  stt s, setS = 42 in
  eff (if s <= 45 then setS (fun s -> s + 1));
  view [()]
;;
view [C ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step five times" ~expected:5 ~actual:steps

let set_in_effect_with_arg_step_one_time () =
  let prog =
    parse_prog
      {|
let C x =
  stt s, setS = 42 in
  eff (if s <> x then setS (fun s -> x));
  view [()]
;;
view [C 42]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step one time" ~expected:1 ~actual:steps

let set_in_effect_with_arg_step_two_times () =
  let prog =
    parse_prog
      {|
let C x =
  stt s, setS = 42 in
  eff (if s <> x then setS (fun s -> x));
  view [()]
;;
view [C 0]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step two times" ~expected:2 ~actual:steps

let set_passed_step_two_times () =
  let prog =
    parse_prog
      {|
let C setS =
  eff (setS (fun s -> 0));
  view [()]
;;
let D x =
  stt s, setS = 42 in
  view [C setS]
;;
view [D ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step two times" ~expected:2 ~actual:steps

let set_passed_step_indefinitely () =
  let prog =
    parse_prog
      {|
let C setS =
  eff (setS (fun s -> s + 1));
  view [()]
;;
let D x =
  stt s, setS = 42 in
  view [C setS]
;;
view [D ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step indefintely" ~expected:fuel ~actual:steps

let set_in_effect_twice_step_one_time () =
  let prog =
    parse_prog
      {|
let C x =
  stt s, setS = 42 in
  eff (setS (fun s -> 43); setS (fun s -> 42));
  view [()]
;;
view [C ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step one time" ~expected:1 ~actual:steps

let set_in_removed_child_step_two_times () =
  let prog =
    parse_prog
      {|
let C x =
  stt s, setS = 42 in
  eff (setS (fun s -> s + 1));
  view [()]
;;
let D x =
  stt s, setS = true in
  eff (setS (fun s -> false));
  if s then
    view [C ()]
  else
    view [()]
;;
view [D ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step two times" ~expected:2 ~actual:steps

let state_persists_in_child () =
  let prog =
    parse_prog
      {|
let C x =
  stt s, setS = 42 in
  eff (setS (fun s -> 0));
  view [()]
;;
let D x =
  stt s, setS = true in
  eff (setS (fun s -> false));
  if s then
    view [C ()]
  else
    view [C ()]
;;
view [D ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step two times" ~expected:2 ~actual:steps

let new_child_steps_again () =
  let prog =
    parse_prog
      {|
let C x =
  stt s, setS = 42 in
  eff (setS (fun s -> 0));
  view [()]
;;
let D x =
  stt s, setS = true in
  eff (setS (fun s -> false));
  if s then
    view [C ()]
  else
    view [C (), C ()]
;;
view [D ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step three times" ~expected:3 ~actual:steps

let set_in_effect_guarded_step_n_times_with_obj () =
  let prog =
    parse_prog
      {|
let C x =
  stt s, setS = (let r = {} in r.x := 42; r) in
  eff (if s.x <= 45 then setS (fun s -> (let r = {} in r.x := s.x + 1; r)));
  view [()]
;;
view [C ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step five times" ~expected:5 ~actual:steps

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
          test_case "obj" `Quick parse_obj;
        ] );
      ( "steps",
        [
          test_case "No side effect should step one time" `Quick no_side_effect;
          test_case "Set in body should not terminate" `Quick
            set_in_body_nonterminate;
          test_case "Guarded set in body should step one time" `Quick
            set_in_body_guarded;
          test_case "Set in effect should step one time" `Quick
            set_in_effect_step_one_time;
          test_case "Set in effect should step two times" `Quick
            set_in_effect_step_two_times;
          test_case "Set in effect should step indefintely" `Quick
            set_in_effect_step_indefinitely;
          test_case "Guarded set in effect should step two times" `Quick
            set_in_effect_guarded_step_two_times;
          test_case "Guarded set in effect should step five times" `Quick
            set_in_effect_guarded_step_n_times;
          test_case "Set in effect with arg should step one time" `Quick
            set_in_effect_with_arg_step_one_time;
          test_case "Set in effect with arg should step two times" `Quick
            set_in_effect_with_arg_step_two_times;
          test_case "Set passed to child should step two times" `Quick
            set_passed_step_two_times;
          test_case "Set passed to child should step indefintely" `Quick
            set_passed_step_indefinitely;
          test_case "Set in effect twice should step one time" `Quick
            set_in_effect_twice_step_one_time;
          test_case "Set in removed child should step two times" `Quick
            set_in_removed_child_step_two_times;
          test_case "Same child gets persisted" `Quick state_persists_in_child;
          test_case "New child steps again" `Quick new_child_steps_again;
          test_case "Guarded set with obj in effect should step five times" `Quick
            set_in_effect_guarded_step_n_times_with_obj;
        ] );
    ]
