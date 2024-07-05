%{
open Syntax
open Prog
open Expr
%}

%token UNIT TRUE FALSE
%token <int> INT
%token <string> ID
%token VIEW
%token FUN LET STT IN EFF
%token IF THEN ELSE
%token EQ /* NOT */
%token AND OR
%token PLUS MINUS TIMES
%token LPAREN RPAREN LBRACK RBRACK
%token RARROW COMMA SEMICOLON COMPEND
%token EOF

%nonassoc THEN /* below ELSE (if ... then ...) */
%nonassoc ELSE /* (if ... then ... else ...) */
%right    OR
%right    AND
%left     PLUS MINUS
%left     TIMES


%start <Prog.t> prog
%%
prog:
    | prog = comp_lst; EOF { prog }
comp_lst:
    | e = expr { Expr (hook_free_exn e) }
    | c = comp_expr; COMPEND; tl = comp_lst { Comp (c, tl) } ;
comp_expr:
    | LET; name = var; param = var; EQ; body = expr { { name; param; body = hook_full body } }
expr:
    | bop_expr { $1 }
    | FUN; param = var; RARROW; body = expr { Ex (Fn { param; body = hook_free_exn body }) }
    | LET; id = var; EQ; bound = expr; IN; body = expr
      { let Ex body = body in
        Ex (Let { id; bound = hook_free_exn bound; body })
      }
    | STT; stt = var; COMMA; set = var; EQ; init = expr; IN; body = expr
      { Ex (Stt { label = -1; stt; set; init = hook_free_exn init; body = hook_full body }) }
    | EFF; e = expr { Ex (Eff (hook_free_exn e)) }
    | VIEW; LBRACK; vss = separated_nonempty_list(SEMICOLON, expr); RBRACK { Ex (View (List.map hook_free_exn vss)) }
    | fn = atom; arg = atom { Ex (App { fn = hook_free_exn fn; arg = hook_free_exn arg }) }
    | IF; pred = expr; THEN; con = expr; ELSE; alt = expr
      { Ex (Cond { pred = hook_free_exn pred; con = hook_free_exn con; alt = hook_free_exn alt }) }
    | IF; pred = expr; THEN; con = expr
      { Ex (Cond { pred = hook_free_exn pred; con = hook_free_exn con; alt = Const Unit }) }
bop_expr:
    | atom { $1 }
    | left = bop_expr; op = op; right = bop_expr
      { Ex (Bin_op { op; left = hook_free_exn left; right = hook_free_exn right }) }
%inline op:
    | AND { And }
    | OR { Or }
    | PLUS { Plus }
    | MINUS { Minus }
    | TIMES { Times }
atom:
    | UNIT { Ex (Const Unit) }
    | TRUE { Ex (Const (Bool true)) }
    | FALSE { Ex (Const (Bool false)) }
    | n = INT { Ex (Const (Int n)) }
    | var = var { Ex (Var var) }
    | LPAREN; e = expr; RPAREN { e }
var:
    | x = ID { x }
