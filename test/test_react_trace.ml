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

let rec alpha_conv_expr_blind :
    type a. (string -> string) -> a Syntax.Expr.t -> a Syntax.Expr.t =
  let open Syntax.Expr in
  fun bindings -> function
    | Const c -> Const c
    | Var x -> Var (bindings x)
    | View es -> View (List.map es ~f:(alpha_conv_expr_blind bindings))
    | Cond { pred; con; alt } ->
        Cond
          {
            pred = alpha_conv_expr_blind bindings pred;
            con = alpha_conv_expr_blind bindings con;
            alt = alpha_conv_expr_blind bindings alt;
          }
    | Fn { param; body } ->
        Fn { param; body = alpha_conv_expr_blind bindings body }
    | App { fn; arg } ->
        App
          {
            fn = alpha_conv_expr_blind bindings fn;
            arg = alpha_conv_expr_blind bindings arg;
          }
    | Let { id; bound; body } ->
        Let
          {
            id = bindings id;
            bound = alpha_conv_expr_blind bindings bound;
            body = alpha_conv_expr_blind bindings body;
          }
    | Stt { stt; set; init; body; label } ->
        Stt
          {
            stt = bindings stt;
            set = bindings set;
            init = alpha_conv_expr_blind bindings init;
            body = alpha_conv_expr_blind bindings body;
            label = bindings (Int.to_string label) |> Int.of_string;
          }
    | Eff e -> Eff (alpha_conv_expr_blind bindings e)
    | Seq (e1, e2) ->
        Seq
          (alpha_conv_expr_blind bindings e1, alpha_conv_expr_blind bindings e2)
    | Bop { left; right; op } ->
        Bop
          {
            left = alpha_conv_expr_blind bindings left;
            right = alpha_conv_expr_blind bindings right;
            op;
          }
    | Uop { arg; op } -> Uop { arg = alpha_conv_expr_blind bindings arg; op }
    | Alloc -> Alloc
    | Set { obj; idx; value } ->
        Set
          {
            obj = alpha_conv_expr_blind bindings obj;
            idx = alpha_conv_expr_blind bindings idx;
            value = alpha_conv_expr_blind bindings value;
          }
    | Get { obj; idx } ->
        Get
          {
            obj = alpha_conv_expr_blind bindings obj;
            idx = alpha_conv_expr_blind bindings idx;
          }

let rec alpha_conv_expr :
    type a.
    (string -> string) -> a Syntax.Expr.t -> a Syntax.Expr.t -> a Syntax.Expr.t
    =
  let open Syntax.Expr in
  fun bindings base src ->
    match (base, src) with
    | Const _, Const _ -> src
    | Var _, Var x' -> Var (bindings x')
    | View es, View es' ->
        let len = List.length es in
        let len' = List.length es' in
        if len < len' then
          View
            (List.map2_exn es (List.take es' len) ~f:(alpha_conv_expr bindings))
        else if len > len' then
          View
            (List.map2_exn (List.take es len') es' ~f:(alpha_conv_expr bindings))
        else View (List.map2_exn es es' ~f:(alpha_conv_expr bindings))
    | Cond { pred; con; alt }, Cond { pred = pred'; con = con'; alt = alt' } ->
        Cond
          {
            pred = alpha_conv_expr bindings pred pred';
            con = alpha_conv_expr bindings con con';
            alt = alpha_conv_expr bindings alt alt';
          }
    | Fn { param; body }, Fn { param = param'; body = body' } ->
        let bindings' x = if String.(x = param') then param else bindings x in
        Fn { param; body = alpha_conv_expr bindings' body body' }
    | App { fn; arg }, App { fn = fn'; arg = arg' } ->
        App
          {
            fn = alpha_conv_expr bindings fn fn';
            arg = alpha_conv_expr bindings arg arg';
          }
    | Let { id; bound; body }, Let { id = id'; bound = bound'; body = body' } ->
        let bindings' x = if String.(x = id') then id else bindings x in
        Let
          {
            id;
            bound = alpha_conv_expr bindings bound bound';
            body = alpha_conv_expr bindings' body body';
          }
    | ( Stt { stt; set; init; body; label },
        Stt
          { stt = stt'; set = set'; init = init'; body = body'; label = label' }
      ) ->
        let bindings' x =
          if String.(x = stt') then stt
          else if String.(x = set') then set
          else if String.(x = Int.to_string label') then Int.to_string label
          else bindings x
        in
        Stt
          {
            stt;
            set;
            init = alpha_conv_expr bindings init init';
            body = alpha_conv_expr bindings' body body';
            label;
          }
    | Eff e, Eff e' -> Eff (alpha_conv_expr bindings e e')
    | Seq (e1, e2), Seq (e1', e2') ->
        Seq (alpha_conv_expr bindings e1 e1', alpha_conv_expr bindings e2 e2')
    | Bop { left; right; _ }, Bop { left = left'; right = right'; op = op' } ->
        Bop
          {
            left = alpha_conv_expr bindings left left';
            right = alpha_conv_expr bindings right right';
            op = op';
          }
    | Uop { arg; _ }, Uop { arg = arg'; op = op' } ->
        Uop { arg = alpha_conv_expr bindings arg arg'; op = op' }
    | Alloc, Alloc -> Alloc
    | Set { obj; idx; value }, Set { obj = obj'; idx = idx'; value = value' } ->
        Set
          {
            obj = alpha_conv_expr bindings obj obj';
            idx = alpha_conv_expr bindings idx idx';
            value = alpha_conv_expr bindings value value';
          }
    | Get { obj; idx }, Get { obj = obj'; idx = idx' } ->
        Get
          {
            obj = alpha_conv_expr bindings obj obj';
            idx = alpha_conv_expr bindings idx idx';
          }
    | _, _ -> alpha_conv_expr_blind bindings src

let rec alpha_conv_prog_blind bindings src =
  let open Syntax.Prog in
  match src with
  | Expr e -> Expr (alpha_conv_expr_blind bindings e)
  | Comp ({ name; param; body }, e) ->
      let bindings' x = if String.(x = name) then name else bindings x in
      Comp
        ( { name; param; body = alpha_conv_expr_blind bindings' body },
          alpha_conv_prog_blind bindings' e )

let rec alpha_conv_prog bindings base src =
  let open Syntax.Prog in
  match (base, src) with
  | Expr e, Expr e' -> Expr (alpha_conv_expr bindings e e')
  | ( Comp ({ name; param; body }, e),
      Comp ({ name = name'; param = param'; body = body' }, e') ) ->
      let body_bindings x = if String.(x = param') then param else bindings x in
      let e_bindings x = if String.(x = name') then name else bindings x in
      Comp
        ( { name; param; body = alpha_conv_expr body_bindings body body' },
          alpha_conv_prog e_bindings e e' )
  | _, _ -> alpha_conv_prog_blind bindings src

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
                   ( Set
                       {
                         obj = Var "x";
                         idx = Const (String "y");
                         value = Const (Int 3);
                       },
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
    ~msg:"convert var" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog "x" |> Prog.sexp_of_t)

let js_literal () =
  let open Syntax in
  let js, _ = parse_js "42; true; null" in
  let prog = Js_syntax.convert js in
  (* null is converted to unit and optimized out *)
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert literal" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog "42; true" |> Prog.sexp_of_t)

let js_jsx () =
  let open Syntax in
  let js, _ = parse_js "<></>; <Comp />" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert jsx" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog "view [()]; view [Comp ()]" |> Prog.sexp_of_t)

let js_op () =
  let open Syntax in
  let js, _ =
    parse_js
      {|
a || b; a && b; a ?? b;
a === b; a !== b; a < b; a <= b; a > b; a >= b;
a + b; a - b; a * b;
void a; -a; +a; !a|}
  in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert operator" ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog
         {|
(let a' = a in if a' then a' else b);
(let a'' = a in if a'' then b else a'');
(let a''' = a in if a''' = () then b else a''');
a = b; a <> b; a < b; a <= b; a > b; a >= b;
a + b; a - b; a * b;
(a; ()); -a; +a; not a|}
      |> alpha_conv_prog Fun.id prog
      |> Prog.sexp_of_t)

let js_optcall () =
  let open Syntax in
  let js, _ = parse_js "a?.(b)" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert optional call" ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog "let a' = a in if a' = () then () else a'(b)"
      |> alpha_conv_prog Fun.id prog
      |> Prog.sexp_of_t)

let js_cond () =
  let open Syntax in
  let js, _ = parse_js "if (a) b; else c;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert conditional" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog "if a then b else c" |> Prog.sexp_of_t)

let js_pattern_id () =
  let open Syntax in
  let js, _ = parse_js "let p = q;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"condition id pattern" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog {|let p = q in ()|} |> Prog.sexp_of_t)

let js_pattern_object () =
  let open Syntax in
  let js, _ = parse_js "let {x, y} = q;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert object pattern" ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog {|
let q' = q in
let x = q'["x"] in
let y = q'["y"] in
()|}
      |> alpha_conv_prog Fun.id prog
      |> Prog.sexp_of_t)

let js_pattern_array () =
  let open Syntax in
  let js, _ = parse_js "let [x, y, , z] = q;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert array pattern" ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog
         {|
let q' = q in
  let x = q'[0] in
  let y = q'[1] in
  let z = q'[3] in
()|}
      |> alpha_conv_prog Fun.id prog
      |> Prog.sexp_of_t)

let js_pattern_nested () =
  let open Syntax in
  let js, _ = parse_js "let {x: {y: [a, b]}} = q;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert nested pattern" ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog
         {|
let q' = q in
let x = q'["x"] in
let y = x["y"] in
let a = y[0] in
let b = y[1] in
()|}
      |> alpha_conv_prog Fun.id prog
      |> Prog.sexp_of_t)

let js_object () =
  let open Syntax in
  let js, _ = parse_js "let p = {y: 1, z: 2, 3: 4}; p.y; p[1+2]" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert object" ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog
         {|
let p = (let obj = {} in obj["y"] := 1; obj["z"] := 2; obj[3] := 4; obj) in p["y"]; p[1+2]|}
      |> alpha_conv_prog Fun.id prog
      |> Prog.sexp_of_t)

let js_if_cpl_same () =
  let open Syntax in
  let js, _ = parse_js "if (a) break A; else break A;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert conditional with same completion" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog {|
      if a then ()
      else ()
    |}
    |> Prog.sexp_of_t)

let js_if_cpl_brk_brk () =
  let open Syntax in
  let js, _ = parse_js "if (a) break A; else break B;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert conditional with break-break completion" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog {|
      if a then (let obj1 = {} in obj1["tag"] := "BRK"; obj1["label"] := "brk:A"; obj1)
      else (let obj2 = {} in obj2["tag"] := "BRK"; obj2["label"] := "brk:B"; obj2)
    |}
    |> alpha_conv_prog Fun.id prog
    |> Prog.sexp_of_t)

let js_if_cpl_brk_nrm () =
  let open Syntax in
  let js, _ = parse_js "if (a) break A; else b" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert conditional with break-normal completion" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog {|
      if a then (let obj1 = {} in obj1["tag"] := "BRK"; obj1["label"] := "brk:A"; obj1)
      else (b; let obj2 = {} in obj2["tag"] := "NRM"; obj2)
    |}
    |> alpha_conv_prog Fun.id prog
    |> Prog.sexp_of_t)

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
          (* completion tests *)
          test_case "if cpl break break" `Quick js_if_cpl_brk_brk;
          test_case "if cpl break normal" `Quick js_if_cpl_brk_nrm;
          test_case "if cpl same" `Quick js_if_cpl_same;
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
