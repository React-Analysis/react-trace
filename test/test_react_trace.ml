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

let parse_js s =
  Parser_flow.program_file ~fail:false
    ~parse_options:
      (Some { Parser_env.default_parse_options with components = true })
    s None

let rec extract_names_in_expr :
    type a. a Syntax.Expr.t -> a Syntax.Expr.t * string list =
  let open Syntax.Expr in
  fun e ->
    match e with
    | Const _ -> (e, [])
    | Var id -> (Var "_", [ id ])
    | View es ->
        let es, names = List.map es ~f:extract_names_in_expr |> List.unzip in
        (View es, List.concat names)
    | Cond { pred; con; alt } ->
        let pred, pred_names = extract_names_in_expr pred in
        let con, con_names = extract_names_in_expr con in
        let alt, alt_names = extract_names_in_expr alt in
        (Cond { pred; con; alt }, pred_names @ con_names @ alt_names)
    | Fn { param; body } ->
        let body, names = extract_names_in_expr body in
        (Fn { param; body }, param :: names)
    | App { fn; arg } ->
        let fn, fn_names = extract_names_in_expr fn in
        let arg, arg_names = extract_names_in_expr arg in
        (App { fn; arg }, fn_names @ arg_names)
    | Let { id; bound; body } ->
        let bound, bound_names = extract_names_in_expr bound in
        let body, body_names = extract_names_in_expr body in
        (Let { id = "_"; bound; body }, (id :: bound_names) @ body_names)
    | Stt { stt; set; init; body; _ } ->
        let init, init_names = extract_names_in_expr init in
        let body, body_names = extract_names_in_expr body in
        ( Stt { stt = "_"; set = "_"; init; body; label = 0 },
          (stt :: set :: init_names) @ body_names )
    | Eff e ->
        let e, names = extract_names_in_expr e in
        (Eff e, names)
    | Seq (e1, e2) ->
        let e1, e1_names = extract_names_in_expr e1 in
        let e2, e2_names = extract_names_in_expr e2 in
        (Seq (e1, e2), e1_names @ e2_names)
    | Bop { left; right; op } ->
        let left, left_names = extract_names_in_expr left in
        let right, right_names = extract_names_in_expr right in
        (Bop { left; right; op }, left_names @ right_names)
    | Uop { arg; op } ->
        let arg, names = extract_names_in_expr arg in
        (Uop { arg; op }, names)
    | Alloc -> (Alloc, [])
    | Set { obj; idx; value } ->
        let obj, obj_names = extract_names_in_expr obj in
        let idx, idx_names = extract_names_in_expr idx in
        let value, value_names = extract_names_in_expr value in
        (Set { obj; idx; value }, obj_names @ idx_names @ value_names)
    | Get { obj; idx } ->
        let obj, obj_names = extract_names_in_expr obj in
        let idx, idx_names = extract_names_in_expr idx in
        (Get { obj; idx }, obj_names @ idx_names)

let rec extract_names_in_prog =
  let open Syntax.Prog in
  function
  | Expr e ->
      let e, names = extract_names_in_expr e in
      (Expr e, names)
  | Comp ({ name; param; body }, e) ->
      let body, body_names = extract_names_in_expr body in
      let e, e_names = extract_names_in_prog e in
      ( Comp ({ name = "_"; param = "_"; body }, e),
        (name :: param :: body_names) @ e_names )

let names_to_numbering names =
  let map =
    List.foldi names ~init:String.Map.empty ~f:(fun i acc name ->
        Map.set acc ~key:name ~data:i)
  in
  List.map names ~f:(Map.find_exn map)

let normalize_prog prog =
  let prog, names = extract_names_in_prog prog in
  let numbering = names_to_numbering names in
  sexp_of_pair Syntax.Prog.sexp_of_t (sexp_of_list sexp_of_int) (prog, numbering)

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
  let (Ex expr) =
    parse_expr
      "let (x, setX) = useState 42 in let (y, setY) = useState -42 in x + y"
  in
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
  let (Ex expr) = parse_expr "useEffect (x ())" in
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
  let (Ex expr) = parse_expr {|let x = {} in x["y"] := 3; x["y"]|} in
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
                   ( Set { obj = Var "x"; idx = Const (String "y"); value = Const (Int 3) },
                     Get { obj = Var "x"; idx = Const (String "y") } );
             }))

let parse_indexing () =
  let open Syntax in
  let (Ex expr) = parse_expr "let x = {} in x[2+2] := 1; x[4] + 1" in
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
                   ( Set
                       {
                         obj = Var "x";
                         idx =
                           Bop
                             {
                               op = Plus;
                               left = Const (Int 2);
                               right = Const (Int 2);
                             };
                         value = Const (Int 1);
                       },
                     Bop
                       {
                         op = Plus;
                         left = Get { obj = Var "x"; idx = Const (Int 4) };
                         right = Const (Int 1);
                       } );
             }))

let parse_string () =
  let open Syntax in
  let (Ex expr) = parse_expr {|"hello world"; "\"\\ hello"|} in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse string" ~actual:(Expr.sexp_of_t expr)
    ~expected:
      Expr.(
        sexp_of_t
          (Seq (Const (String "hello world"), Const (String "\"\\ hello"))))

let js_var () =
  let open Syntax in
  let js, _ = parse_js "x" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse obj" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog "x; ()" |> Prog.sexp_of_t)

let js_literal () =
  let open Syntax in
  let js, _ = parse_js "42; true; null" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse obj" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog "42; true; (); ()" |> Prog.sexp_of_t)

let js_jsx () =
  let open Syntax in
  let js, _ = parse_js "<></>; <Comp />" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse obj" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog "view [()]; view [Comp ()]; ()" |> Prog.sexp_of_t)

let js_op () =
  let js, _ =
    parse_js
      "a || b; a && b; a ?? b; \
       a === b; a !== b; a < b; a <= b; a > b; a >= b; a + b; \
       a - b; a * b; void a; -a; +a; !a"
  in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse obj" ~actual:(normalize_prog prog)
    ~expected:
      (parse_prog
         "(let a' = a in if a' then a' else b); \
          (let a'' = a in if a'' then b else a''); \
          (let a''' = a in if a''' = () then b else a'''); \
          a = b; a <> b; a < b; \
          a <= b; a > b; a >= b; a + b; a - b; a * b; (a; ()); -a; +a; not a; \
          ()"
      |> normalize_prog)

let js_optcall () =
  let js, _ = parse_js "a?.(b)" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse obj" ~actual:(normalize_prog prog)
    ~expected:
      (parse_prog "(let a' = a in if a' = () then () else a'(b)); ()"
      |> normalize_prog)

let js_cond () =
  let js, _ = parse_js "if (a) b; else c;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse obj"
    ~actual:(Syntax.Prog.sexp_of_t prog)
    ~expected:
      (parse_prog "if a then (b; ()) else (c; ()); ()" |> Syntax.Prog.sexp_of_t)

let js_pattern_id () =
  let js, _ = parse_js "let p = q;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse obj"
    ~actual:(Syntax.Prog.sexp_of_t prog)
    ~expected:(parse_prog {|let p = q in ()|} |> Syntax.Prog.sexp_of_t)

let js_pattern_object () =
  let js, _ = parse_js "let {x, y} = q;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse obj" ~actual:(normalize_prog prog)
    ~expected:
      (parse_prog {|
let q' = q in
let x = q'["x"] in
let y = q'["y"] in
()|}
      |> normalize_prog)

let js_pattern_array () =
  let js, _ = parse_js "let [x, y, , z] = q;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse obj" ~actual:(normalize_prog prog)
    ~expected:
      (parse_prog
         {|
let q' = q in
  let x = q'[0] in
  let y = q'[1] in
  let z = q'[3] in
()|}
      |> normalize_prog)

let js_pattern_nested () =
  let js, _ = parse_js "let {x: {y: [a, b]}} = q;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse obj" ~actual:(normalize_prog prog)
    ~expected:
      (parse_prog
         {|
let q' = q in
let x = q'["x"] in
let y = x["y"] in
let a = y[0] in
let b = y[1] in
()|}
      |> normalize_prog)

let js_object () =
  (* Without parentheses, the object is parsed as a block with two labeled
     statements *)
  let js, _ = parse_js "let p = {y: 1, z: 2, 3: 4}; p.y; p[1+2]" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"parse obj" ~actual:(normalize_prog prog)
    ~expected:
      (parse_prog
         {|
let p = (let obj = {} in obj["y"] := 1; obj["z"] := 2; obj[3] := 4; obj) in p["y"]; p[1+2]; ()|}
      |> normalize_prog)

let no_side_effect () =
  let prog =
    parse_prog
      {|
let C x =
  let (s, setS) = useState 42 in
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
  let (s, setS) = useState 42 in
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
  let (s, setS) = useState 42 in
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
  let (s, setS) = useState 42 in
  useEffect (setS (fun s -> 42));
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
  let (s, setS) = useState 42 in
  useEffect (setS (fun s -> 43));
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
  let (s, setS) = useState 42 in
  useEffect (setS (fun s -> s + 1));
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
  let (s, setS) = useState 42 in
  useEffect (if s = 42 then setS (fun s -> 43));
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
  let (s, setS) = useState 42 in
  useEffect (if s <= 45 then setS (fun s -> s + 1));
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
  let (s, setS) = useState 42 in
  useEffect (if s <> x then setS (fun s -> x));
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
  let (s, setS) = useState 42 in
  useEffect (if s <> x then setS (fun s -> x));
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
  useEffect (setS (fun s -> 0));
  view [()]
;;
let D x =
  let (s, setS) = useState 42 in
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
  useEffect (setS (fun s -> s + 1));
  view [()]
;;
let D x =
  let (s, setS) = useState 42 in
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
  let (s, setS) = useState 42 in
  useEffect (setS (fun s -> 43); setS (fun s -> 42));
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
  let (s, setS) = useState 42 in
  useEffect (setS (fun s -> s + 1));
  view [()]
;;
let D x =
  let (s, setS) = useState true in
  useEffect (setS (fun s -> false));
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
  let (s, setS) = useState 42 in
  useEffect (setS (fun s -> 0));
  view [()]
;;
let D x =
  let (s, setS) = useState true in
  useEffect (setS (fun s -> false));
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
  let (s, setS) = useState 42 in
  useEffect (setS (fun s -> 0));
  view [()]
;;
let D x =
  let (s, setS) = useState true in
  useEffect (setS (fun s -> false));
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
  let (s, setS) = useState (let r = {} in r["x"] := 42; r) in
  useEffect (if s["x"] <= 45 then setS (fun s -> (let r = {} in r["x"] := s["x"] + 1; r)));
  view [()]
;;
view [C ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step five times" ~expected:5 ~actual:steps

let updating_obj_without_set_does_not_rerender () =
  let prog =
    parse_prog
      {|
let C x =
  let (s, setS) = useState (let r = {} in r["x"] := 42; r) in
  useEffect (s["x"] := 43);
  view [()]
;;
view [C ()]
|}
  in
  let { Interp.steps; _ } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step one time" ~expected:1 ~actual:steps

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
          test_case "indexing" `Quick parse_indexing;
          test_case "string" `Quick parse_string;
        ] );
      ( "convert",
        [
          test_case "var" `Quick js_var;
          test_case "literal" `Quick js_literal;
          test_case "jsx" `Quick js_jsx;
          test_case "binop" `Quick js_op;
          test_case "optcall" `Quick js_optcall;
          test_case "cond" `Quick js_cond;
          test_case "id pattern" `Quick js_pattern_id;
          test_case "object pattern" `Quick js_pattern_object;
          test_case "array pattern" `Quick js_pattern_array;
          test_case "nested pattern" `Quick js_pattern_nested;
          test_case "object" `Quick js_object;
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
          test_case "Guarded set with obj in effect should step five times"
            `Quick set_in_effect_guarded_step_n_times_with_obj;
          test_case "Updating object without set should step one time" `Quick
            updating_obj_without_set_does_not_rerender;
        ] );
    ]
