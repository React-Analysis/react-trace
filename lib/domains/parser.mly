%{
open Syntax
open Prog
open Expr

let make_loc (startpos, endpos) =
  Location.{ loc_start = startpos; loc_end = endpos; loc_ghost = false }

let mkexp ~loc d = Expr.mk ~loc:(make_loc loc) d

let rec label_stts_prog = function
  | Expr _ as e -> e
  | Comp (c, tl) ->
      Comp ({ c with body = label_stts_expr 0 c.body }, label_stts_prog tl)

and label_stts_expr label = function
  | { desc = Stt s; loc } ->
      Expr.mk ~loc
        (Stt { s with label; body = label_stts_expr (label + 1) s.body })
  | e -> e
%}

%token UNIT TRUE FALSE
%token <int> INT
%token <string> ID
%token <string> STRING
%token RECORD ASSIGN
%token VIEW
%token FUN LET STT IN EFF
%token IF THEN ELSE
%token NOT EQ LT GT NE LE GE
%token AND OR
%token PLUS MINUS TIMES
%token LPAREN RPAREN LBRACK RBRACK
%token RARROW COMMA SEMI SEMISEMI
%token EOF

%nonassoc RARROW
%nonassoc IN
%right    SEMI
%nonassoc EFF
%nonassoc THEN /* below ELSE (if ... then ...) */
%nonassoc ELSE /* (if ... then ... else ...) */
%nonassoc ASSIGN
%right    OR
%right    AND
%left     EQ LT GT NE LE GE
%left     PLUS MINUS
%left     TIMES
%nonassoc prec_unary

%start <Prog.t> prog
%start <some_expr> expr
%%

%inline mkexp(symb): symb { Ex (mkexp ~loc:$sloc $1) }

prog:
    | prog = comp_lst; EOF { label_stts_prog prog }
expr:
    | e = expr_; EOF { Ex (label_stts_expr 0 (hook_full e)) }
comp_lst:
    | e = expr_ { Expr (hook_free_exn e) }
    | c = comp_expr; SEMISEMI; tl = comp_lst
      { Comp (c, tl) } ;
comp_expr:
    | LET; name = var; param = var; EQ; body = expr_ { { name; param; body = hook_full body } }
expr_:
    | apply { $1 }
    | mkexp(FUN; param = var; RARROW; body = expr_
      { Fn { param; body = hook_free_exn body; self = None } })
      { $1 }
    | LET; id = var; EQ; bound = expr_; IN; body = expr_
      { let Ex body = body in
        Ex (mkexp ~loc:$sloc (Let { id; bound = hook_free_exn bound; body }))
      }
    | mkexp(LET; LPAREN; stt = var; COMMA; set = var; RPAREN; EQ; STT; init = expr_; IN; body = expr_
      { Stt { label = -1; stt; set; init = hook_free_exn init; body = hook_full body } })
      { $1 }
    | mkexp(EFF; e = expr_
      { Eff (hook_free_exn e) })
      { $1 }
    | mkexp(VIEW; LBRACK; vss = separated_nonempty_list(COMMA, expr_); RBRACK
      { View (List.map hook_free_exn vss) })
      { $1 }
    | mkexp(IF; pred = expr_; THEN; con = expr_; ELSE; alt = expr_
      { Cond { pred = hook_free_exn pred; con = hook_free_exn con; alt = hook_free_exn alt } })
      { $1 }
    | mkexp(IF; pred = expr_; THEN; con = expr_
      { Cond { pred = hook_free_exn pred; con = hook_free_exn con; alt = Expr.mk ~loc:Location.none (Const Unit) } })
      { $1 }
    | e1 = expr_; SEMI; e2 = expr_
      { match hook_free e1, hook_free e2 with
        | Some e1, Some e2 -> Ex (mkexp ~loc:$sloc (Seq (e1, e2)))
        | _, _ -> Ex (mkexp ~loc:$sloc (Seq (hook_full e1, hook_full e2)))
      }
    | mkexp(op = uop; expr_ = expr_ %prec prec_unary
      { Uop { op; arg = hook_free_exn expr_ } })
      { $1 }
    | mkexp(left = expr_; op = bop; right = expr_
      { Bop { op; left = hook_free_exn left; right = hook_free_exn right } })
      { $1 }
    | mkexp(obj = apply; LBRACK; index = expr_; RBRACK
      { Get { obj = hook_free_exn obj; idx = hook_free_exn index } })
      { $1 }
    | mkexp(obj = apply; LBRACK; index = expr_; RBRACK; ASSIGN; value = expr_
      { Set { obj = hook_free_exn obj; idx = hook_free_exn index; value = hook_free_exn value } })
      { $1 }
%inline uop:
    | NOT { Not }
    | PLUS { Uplus }
    | MINUS { Uminus }
%inline bop:
    | EQ { Eq }
    | LT { Lt }
    | GT { Gt }
    | NE { Ne }
    | LE { Le }
    | GE { Ge }
    | AND { And }
    | OR { Or }
    | PLUS { Plus }
    | MINUS { Minus }
    | TIMES { Times }
apply:
    | atom { $1 }
    | mkexp(fn = apply; arg = atom
      { App { fn = hook_free_exn fn; arg = hook_free_exn arg } })
      { $1 }
atom:
    | mkexp(UNIT { Const Unit }) { $1 }
    | mkexp(TRUE { Const (Bool true) }) { $1 }
    | mkexp(FALSE { Const (Bool false) }) { $1 }
    | mkexp(n = INT { Const (Int n) }) { $1 }
    | mkexp(s = STRING { Const (String s) }) { $1 }
    | mkexp(var = var { Var var }) { $1 }
    | mkexp(RECORD { Alloc }) { $1 }
    | LPAREN; e = expr_; RPAREN { e }
var:
    | x = ID { x }
