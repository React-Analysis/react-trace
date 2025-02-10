open! Base
open React_trace
open Lib_domains

let max_fuel = 100

let run prog =
  let open Interp in
  let { steps; _ } =
    run ~fuel:max_fuel ~recorder:(module Default_recorder) prog
  in
  steps

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

let rec alpha_conv_expr_blind : type a.
    (string -> string) -> a Syntax.Expr.desc -> a Syntax.Expr.desc =
  let open Syntax.Expr in
  let alpha_conv_expr_blind' bindings { desc; loc } =
    { desc = alpha_conv_expr_blind bindings desc; loc }
  in
  fun bindings -> function
    | Const c -> Const c
    | Var x -> Var (bindings x)
    | View es -> View (List.map es ~f:(alpha_conv_expr_blind' bindings))
    | Cond { pred; con; alt } ->
        Cond
          {
            pred = alpha_conv_expr_blind' bindings pred;
            con = alpha_conv_expr_blind' bindings con;
            alt = alpha_conv_expr_blind' bindings alt;
          }
    | Fn { self; param; body } ->
        Fn
          {
            self = Option.map ~f:bindings self;
            param;
            body = alpha_conv_expr_blind' bindings body;
          }
    | App { fn; arg } ->
        App
          {
            fn = alpha_conv_expr_blind' bindings fn;
            arg = alpha_conv_expr_blind' bindings arg;
          }
    | Let { id; bound; body } ->
        Let
          {
            id = bindings id;
            bound = alpha_conv_expr_blind' bindings bound;
            body = alpha_conv_expr_blind' bindings body;
          }
    | Stt { stt; set; init; body; label } ->
        Stt
          {
            stt = bindings stt;
            set = bindings set;
            init = alpha_conv_expr_blind' bindings init;
            body = alpha_conv_expr_blind' bindings body;
            label = bindings (Int.to_string label) |> Int.of_string;
          }
    | Eff e -> Eff (alpha_conv_expr_blind' bindings e)
    | Seq (e1, e2) ->
        Seq
          ( alpha_conv_expr_blind' bindings e1,
            alpha_conv_expr_blind' bindings e2 )
    | Bop { left; right; op } ->
        Bop
          {
            left = alpha_conv_expr_blind' bindings left;
            right = alpha_conv_expr_blind' bindings right;
            op;
          }
    | Uop { arg; op } -> Uop { arg = alpha_conv_expr_blind' bindings arg; op }
    | Alloc -> Alloc
    | Set { obj; idx; value } ->
        Set
          {
            obj = alpha_conv_expr_blind' bindings obj;
            idx = alpha_conv_expr_blind' bindings idx;
            value = alpha_conv_expr_blind' bindings value;
          }
    | Get { obj; idx } ->
        Get
          {
            obj = alpha_conv_expr_blind' bindings obj;
            idx = alpha_conv_expr_blind' bindings idx;
          }

let rec alpha_conv_expr : type a.
    (string -> string) -> a Syntax.Expr.t -> a Syntax.Expr.t -> a Syntax.Expr.t
    =
  let open Syntax.Expr in
  fun bindings base src ->
    let { desc = base_desc; _ } = base in
    let { desc = src_desc; loc } = src in
    let desc =
      (match (base_desc, src_desc) with
       | Const _, Const _ -> src_desc
       | Var _, Var x' -> Var (bindings x')
       | View es, View es' ->
           let len = List.length es in
           let len' = List.length es' in
           if len < len' then
             View
               (List.map2_exn es (List.take es' len)
                  ~f:(alpha_conv_expr bindings))
           else if len > len' then
             View
               (List.map2_exn (List.take es len') es'
                  ~f:(alpha_conv_expr bindings))
           else View (List.map2_exn es es' ~f:(alpha_conv_expr bindings))
       | Cond { pred; con; alt }, Cond { pred = pred'; con = con'; alt = alt' }
         ->
           Cond
             {
               pred = alpha_conv_expr bindings pred pred';
               con = alpha_conv_expr bindings con con';
               alt = alpha_conv_expr bindings alt alt';
             }
       | ( Fn { self = None; param; body },
           Fn { self = None; param = param'; body = body' } ) ->
           let bindings' x =
             if String.(x = param') then param else bindings x
           in
           Fn
             { self = None; param; body = alpha_conv_expr bindings' body body' }
       | ( Fn { self = Some self; param; body },
           Fn { self = Some self'; param = param'; body = body' } ) ->
           (* The function name is bound before the parameters according to js
              semantics (ECMA-262 14th edition, p.347, "15.2.5 Runtime
              Semantics: InstantiateOrdinaryFunctionExpression": "5. Perform !
              funcEnv.CreateImmutableBinding(name, false)."). Thus param takes
              precedence over self. *)
           let bindings' x =
             if String.(x = param') then param
             else if String.(x = self') then self
             else bindings x
           in
           Fn
             {
               self = Some self;
               param;
               body = alpha_conv_expr bindings' body body';
             }
       | App { fn; arg }, App { fn = fn'; arg = arg' } ->
           App
             {
               fn = alpha_conv_expr bindings fn fn';
               arg = alpha_conv_expr bindings arg arg';
             }
       | Let { id; bound; body }, Let { id = id'; bound = bound'; body = body' }
         ->
           let bindings' x = if String.(x = id') then id else bindings x in
           Let
             {
               id;
               bound = alpha_conv_expr bindings bound bound';
               body = alpha_conv_expr bindings' body body';
             }
       | ( Stt { stt; set; init; body; label },
           Stt
             {
               stt = stt';
               set = set';
               init = init';
               body = body';
               label = label';
             } ) ->
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
       | Bop { left; right; _ }, Bop { left = left'; right = right'; op = op' }
         ->
           Bop
             {
               left = alpha_conv_expr bindings left left';
               right = alpha_conv_expr bindings right right';
               op = op';
             }
       | Uop { arg; _ }, Uop { arg = arg'; op = op' } ->
           Uop { arg = alpha_conv_expr bindings arg arg'; op = op' }
       | Alloc, Alloc -> Alloc
       | Set { obj; idx; value }, Set { obj = obj'; idx = idx'; value = value' }
         ->
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
       | _, _ -> alpha_conv_expr_blind bindings src_desc
        : a desc)
    in
    { desc; loc }

let rec alpha_conv_prog_blind bindings src =
  let open Syntax.Prog in
  let alpha_conv_expr_blind' bindings e =
    let open Syntax.Expr in
    { desc = alpha_conv_expr_blind bindings e.desc; loc = e.loc }
  in
  match src with
  | Expr e -> Expr (alpha_conv_expr_blind' bindings e)
  | Comp ({ name; param; body }, e) ->
      let bindings' x = if String.(x = name) then name else bindings x in
      Comp
        ( { name; param; body = alpha_conv_expr_blind' bindings' body },
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

let e_const : 'a. Syntax.Expr.const -> 'a Syntax.Expr.t =
 fun c -> { desc = Const c; loc = Location.none }

let e_var v = Syntax.Expr.{ desc = Var v; loc = Location.none }
let e_app fn arg = Syntax.Expr.{ desc = App { fn; arg }; loc = Location.none }

let e_fn ?self param body =
  Syntax.Expr.{ desc = Fn { self; param; body }; loc = Location.none }

let e_let id bound body =
  Syntax.Expr.{ desc = Let { id; bound; body }; loc = Location.none }

let e_cond pred con alt =
  Syntax.Expr.{ desc = Cond { pred; con; alt }; loc = Location.none }

let e_bop op left right =
  Syntax.Expr.{ desc = Bop { op; left; right }; loc = Location.none }

let e_uop op arg = Syntax.Expr.{ desc = Uop { op; arg }; loc = Location.none }

let e_seq left right =
  Syntax.Expr.{ desc = Seq (left, right); loc = Location.none }

let e_eff expr = Syntax.Expr.{ desc = Eff expr; loc = Location.none }

let e_stt label stt set init body =
  Syntax.Expr.
    { desc = Stt { label; stt; set; init; body }; loc = Location.none }

let e_view elements = Syntax.Expr.{ desc = View elements; loc = Location.none }

let e_set obj idx value =
  Syntax.Expr.{ desc = Set { obj; idx; value }; loc = Location.none }

let e_get obj idx = Syntax.Expr.{ desc = Get { obj; idx }; loc = Location.none }
let e_alloc = Syntax.Expr.{ desc = Alloc; loc = Location.none }

let parse_expr_test msg input expected =
  let open Syntax in
  let (Ex expr) = parse_expr input in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg ~actual:(Expr.sexp_of_t expr) ~expected:(Expr.sexp_of_t expected)

let parse_unit () = parse_expr_test "parse unit" "()" (e_const Unit)
let parse_true () = parse_expr_test "parse true" "true" (e_const (Bool true))

let parse_false () =
  parse_expr_test "parse false" "false" (e_const (Bool false))

let parse_int () = parse_expr_test "parse int" "42" (e_const (Int 42))

let parse_var () =
  parse_expr_test "parse var" "_some_variable123" (e_var "_some_variable123")

let parse_view () =
  parse_expr_test "parse view" "view [(), 42, (), Comp ()]"
    (e_view
       [
         e_const Unit;
         e_const (Int 42);
         e_const Unit;
         e_app (e_var "Comp") (e_const Unit);
       ])

let parse_open_cond () =
  parse_expr_test "parse open cond" "if true then if true then ()"
    (e_cond (e_const (Bool true))
       (e_cond (e_const (Bool true)) (e_const Unit) (e_const Unit))
       (e_const Unit))

let parse_closed_cond () =
  parse_expr_test "parse closed cond" "if true then if true then () else ()"
    (e_cond (e_const (Bool true))
       (e_cond (e_const (Bool true)) (e_const Unit) (e_const Unit))
       (e_const Unit))

let parse_fn () =
  parse_expr_test "parse function" "fun x -> fun y -> x + y"
    (e_fn "x" (e_fn "y" (e_bop Plus (e_var "x") (e_var "y"))))

let parse_rec () =
  parse_expr_test "parse recursive function" "rec f = fun x -> f x"
    (e_fn ~self:"f" "x" (e_app (e_var "f") (e_var "x")))

let parse_app () =
  parse_expr_test "parse app" "a b c"
    (e_app (e_app (e_var "a") (e_var "b")) (e_var "c"))

let parse_let () =
  parse_expr_test "parse let" "let x = let y = 1 in y in let z = x in z"
    (e_let "x"
       (e_let "y" (e_const (Int 1)) (e_var "y"))
       (e_let "z" (e_var "x") (e_var "z")))

let parse_stt () =
  parse_expr_test "parse stt"
    "let (x, setX) = useState 42 in let (y, setY) = useState -42 in x + y"
    (e_stt 0 "x" "setX" (e_const (Int 42))
       (e_stt 1 "y" "setY"
          (e_uop Uminus (e_const (Int 42)))
          (e_bop Plus (e_var "x") (e_var "y"))))

let parse_eff () =
  parse_expr_test "parse eff" "useEffect (x ())"
    (e_eff (e_app (e_var "x") (e_const Unit)))

let parse_seq () =
  parse_expr_test "parse seq" "a; b; c; d"
    (e_seq (e_var "a") (e_seq (e_var "b") (e_seq (e_var "c") (e_var "d"))))

let parse_op () =
  parse_expr_test "parse op" "not (+-a () <= 0 + -0) || true"
    (e_bop Or
       (e_uop Not
          (e_bop Le
             (e_uop Uplus (e_uop Uminus (e_app (e_var "a") (e_const Unit))))
             (e_bop Plus (e_const (Int 0)) (e_uop Uminus (e_const (Int 0))))))
       (e_const (Bool true)))

let parse_obj () =
  parse_expr_test "parse obj" "let x = {} in x[\"y\"] := 3; x[\"y\"]"
    (e_let "x" e_alloc
       (e_seq
          (e_set (e_var "x") (e_const (String "y")) (e_const (Int 3)))
          (e_get (e_var "x") (e_const (String "y")))))

let parse_indexing () =
  parse_expr_test "parse obj" "let x = {} in x[2+2] := 1; x[4] + 1"
    (e_let "x" e_alloc
       (e_seq
          (e_set (e_var "x")
             (e_bop Plus (e_const (Int 2)) (e_const (Int 2)))
             (e_const (Int 1)))
          (e_bop Plus (e_get (e_var "x") (e_const (Int 4))) (e_const (Int 1)))))

let parse_string () =
  parse_expr_test "parse string" "\"hello world\""
    (e_const (String "hello world"))

let js_var () =
  let open Syntax in
  let js, _ = parse_js "x" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert var" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog "x" |> Prog.sexp_of_t)

let js_fn () =
  let open Syntax in
  let js, _ = parse_js "let t = (function(x) {})" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert function" ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog "let t = fun x -> () in ()" |> Prog.sexp_of_t)

let js_rec () =
  let open Syntax in
  let js, _ = parse_js "let t = (function f(x) { return f(x); })" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert recursive function" ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog "let t = (rec f = fun x -> f x) in ()" |> Prog.sexp_of_t)

let js_while () =
  let open Syntax in
  let js, _ =
    parse_js "let a = true; let b = (function(x){}); while (a) { b(0) }"
  in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert while" ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog
         {|
let a = true in
let b = fun x -> () in
(rec Fbrk = fun Xbrk ->
  let Cbrk = (rec Fcont = fun Xcont ->
      let Ccont =
        let Cif =
          if a then
            let Ctrue = {} in
            Ctrue["tag"] := "NRM";
            Ctrue
          else
            let Cfalse = {} in
            Cfalse["tag"] := "BRK";
            Cfalse["label"] := brk;
            Cfalse
        in
        if Cif["tag"] = "NRM" then (
          b 0;
          let Cbody = {} in
          Cbody["tag"] := "NRM";
          Cbody
        ) else
          Cif
      in
      if  Ccont["tag"] = "BRK" &&
          Ccont["label"] = "con" then
        let CFnrm = {} in
        CFnrm["tag"] := "NRM";
        CFnrm
      else if Ccont["tag"] = NRM then
        Fcont ()
      else
        Ccont)
    ()
  in
  if  Cbrk["tag"] = "BRK" &&
      Cbrk["label"] = "brk" then
    let CFnrm2 = {} in
    CFnrm2["tag"] := "NRM";
    CFnrm2
  else if Cbrk["tag"] = "NRM" then
    Fbrk ()
  else Cbrk)
()
|}
      |> alpha_conv_prog Fn.id prog |> Prog.sexp_of_t)

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
  let js, _ = parse_js "<></>; <Comp />; <Mod.Comp />" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert jsx" ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog {|view [()]; view [Comp ()]; view [(Mod["Comp"]) ()]|}
      |> Prog.sexp_of_t)

let js_op () =
  let open Syntax in
  let js, _ =
    parse_js
      {|
a || b; a && b; a ?? b;
a === b; a !== b; a < b; a <= b; a > b; a >= b;
a + b; a - b; a * b;
void a; -a; +a; !a
|}
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
(a; ()); -a; +a; not a
|}
      |> alpha_conv_prog Fn.id prog |> Prog.sexp_of_t)

let js_optcall () =
  let open Syntax in
  let js, _ = parse_js "a?.(b)" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert optional call" ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog "let a' = a in if a' = () then () else a'(b)"
      |> alpha_conv_prog Fn.id prog |> Prog.sexp_of_t)

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
()
|}
      |> alpha_conv_prog Fn.id prog |> Prog.sexp_of_t)

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
()
|}
      |> alpha_conv_prog Fn.id prog |> Prog.sexp_of_t)

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
()
|}
      |> alpha_conv_prog Fn.id prog |> Prog.sexp_of_t)

let js_object () =
  let open Syntax in
  let js, _ = parse_js "let p = {y: 1, z: 2, 3: 4}; p.y; p[1+2]" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert object" ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog
         {|
let p = (let obj = {} in obj["y"] := 1; obj["z"] := 2; obj[3] := 4; obj) in p["y"]; p[1+2]
|}
      |> alpha_conv_prog Fn.id prog |> Prog.sexp_of_t)

let js_if_cpl_same () =
  let open Syntax in
  let js, _ = parse_js "if (a) break A; else break A;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert conditional with same completion"
    ~actual:(Prog.sexp_of_t prog)
    ~expected:(parse_prog {|
if a then ()
else ()
|} |> Prog.sexp_of_t)

let js_if_cpl_brk_brk () =
  let open Syntax in
  let js, _ = parse_js "if (a) break A; else break B;" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert conditional with break-break completion"
    ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog
         {|
if a then (let obj1 = {} in obj1["tag"] := "BRK"; obj1["label"] := "brk:A"; obj1)
else (let obj2 = {} in obj2["tag"] := "BRK"; obj2["label"] := "brk:B"; obj2)
|}
      |> alpha_conv_prog Fn.id prog |> Prog.sexp_of_t)

let js_if_cpl_brk_nrm () =
  let open Syntax in
  let js, _ = parse_js "if (a) break A; else b" in
  let prog = Js_syntax.convert js in
  Alcotest.(check' (of_pp Sexp.pp_hum))
    ~msg:"convert conditional with break-normal completion"
    ~actual:(Prog.sexp_of_t prog)
    ~expected:
      (parse_prog
         {|
if a then (let obj1 = {} in obj1["tag"] := "BRK"; obj1["label"] := "brk:A"; obj1)
else (b; let obj2 = {} in obj2["tag"] := "NRM"; obj2)
|}
      |> alpha_conv_prog Fn.id prog |> Prog.sexp_of_t)

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
  let steps = run prog in
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
    Interp.re_render_limit_h run prog ~re_render_limit:25 |> ignore
  in
  Alcotest.(check_raises)
    "retry indefintely" Interp_effects.Too_many_re_renders run

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
  let steps = run prog in
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
  let steps = run prog in
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
  let steps = run prog in
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
  let steps = run prog in
  Alcotest.(check' int) ~msg:"step indefintely" ~expected:max_fuel ~actual:steps

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
  let steps = run prog in
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
  let steps = run prog in
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
  let steps = run prog in
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
  let steps = run prog in
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
  let steps = run prog in
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
  let steps = run prog in
  Alcotest.(check' int) ~msg:"step indefintely" ~expected:max_fuel ~actual:steps

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
  let steps = run prog in
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
  let steps = run prog in
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
  let steps = run prog in
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
  let steps = run prog in
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
  let steps = run prog in
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
  let steps = run prog in
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
          test_case "rec" `Quick parse_rec;
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
          test_case "function" `Quick js_fn;
          test_case "recursive function" `Quick js_rec;
          test_case "while" `Quick js_while;
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
