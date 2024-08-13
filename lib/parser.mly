%{
open Syntax
open Prog
open Expr

let rec label_stts_prog = function
  | Expr _ as e -> e
  | Comp (c, tl) ->
      Comp ({ c with body = label_stts_expr 0 c.body }, label_stts_prog tl)

and label_stts_expr label = function
  | Stt s -> Stt { s with label; body = label_stts_expr (label + 1) s.body }
  | e -> e

%}

%token UNIT TRUE FALSE
%token <int> INT
%token <string> ID
%token RECORD DOT ASSIGN
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
%nonassoc ASSIGN
%nonassoc EFF
%nonassoc THEN /* below ELSE (if ... then ...) */
%nonassoc ELSE /* (if ... then ... else ...) */
%right    OR
%right    AND
%left     EQ LT GT NE LE GE
%left     PLUS MINUS
%left     TIMES
%nonassoc prec_unary

%start <Prog.t> prog
%start <some_expr> expr
%%
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
    | FUN; param = var; RARROW; body = expr_ { Ex (Fn { param; body = hook_free_exn body }) }
    | LET; id = var; EQ; bound = expr_; IN; body = expr_
      { let Ex body = body in
        Ex (Let { id; bound = hook_free_exn bound; body })
      }
    | STT; stt = var; COMMA; set = var; EQ; init = expr_; IN; body = expr_
      { Ex (Stt { label = -1; stt; set; init = hook_free_exn init; body = hook_full body }) }
    | EFF; e = expr_ { Ex (Eff (hook_free_exn e)) }
    | VIEW; LBRACK; vss = separated_nonempty_list(COMMA, expr_); RBRACK { Ex (View (List.map hook_free_exn vss)) }
    | IF; pred = expr_; THEN; con = expr_; ELSE; alt = expr_
      { Ex (Cond { pred = hook_free_exn pred; con = hook_free_exn con; alt = hook_free_exn alt }) }
    | IF; pred = expr_; THEN; con = expr_
      { Ex (Cond { pred = hook_free_exn pred; con = hook_free_exn con; alt = Const Unit }) }
    | e1 = expr_; SEMI; e2 = expr_
      { match hook_free e1, hook_free e2 with
        | Some e1, Some e2 -> Ex (Seq (e1, e2))
        | _, _ -> Ex (Seq (hook_full e1, hook_full e2))
      }
    | op = uop; expr_ = expr_ %prec prec_unary { Ex (Uop { op; arg = hook_free_exn expr_ }) }
    | left = expr_; op = bop; right = expr_
      { Ex (Bop { op; left = hook_free_exn left; right = hook_free_exn right }) }
    | RECORD { Ex (Alloc) }
    | obj = expr_; DOT; field = var { Ex (Get { obj = hook_free_exn obj; field }) }
    | obj = expr_; DOT; field = var; ASSIGN; value = expr_
      { Ex (Set { obj = hook_free_exn obj; field; value = hook_free_exn value }) }
    | obj = expr_; LBRACK; index = expr_; RBRACK { Ex (GetIdx { obj = hook_free_exn obj; idx = hook_free_exn index }) }
    | obj = expr_; LBRACK; index = expr_; RBRACK; ASSIGN; value = expr_
      { Ex (SetIdx { obj = hook_free_exn obj; idx = hook_free_exn index; value = hook_free_exn value }) }
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
    | fn = apply; arg = atom { Ex (App { fn = hook_free_exn fn; arg = hook_free_exn arg }) }
atom:
    | UNIT { Ex (Const Unit) }
    | TRUE { Ex (Const (Bool true)) }
    | FALSE { Ex (Const (Bool false)) }
    | n = INT { Ex (Const (Int n)) }
    | var = var { Ex (Var var) }
    | LPAREN; e = expr_; RPAREN { e }
var:
    | x = ID { x }
