open! Base
open Lib_domains

exception NotImplemented
exception Unreachable

type js_ast = (Loc.t, Loc.t) Flow_ast.Program.t

let parse (filename : string) : js_ast * (Loc.t * Parse_error.t) list =
  let contents = Stdio.In_channel.read_all filename in

  Parser_flow.program_file ~fail:false
    ~parse_options:
      (Some { Parser_env.default_parse_options with components = true })
    contents (Some (File_key.SourceFile filename))

let show (js_ast : js_ast) : string = Flow_ast.Program.show Loc.pp Loc.pp js_ast

let fresh_id =
  let counter = ref 0 in
  fun () ->
    let n = !counter in
    Int.incr counter;
    "@@" ^ Int.to_string n

let fresh_label =
  let counter = ref 0 in
  fun () ->
    let n = !counter in
    Int.incr counter;
    n

let convert_bop (bop : Flow_ast.Expression.Binary.operator) : Syntax.Expr.bop =
  let open Syntax.Expr in
  match bop with
  | Equal -> raise NotImplemented
  | NotEqual -> raise NotImplemented
  | StrictEqual -> Eq
  | StrictNotEqual -> Ne
  | LessThan -> Lt
  | LessThanEqual -> Le
  | GreaterThan -> Gt
  | GreaterThanEqual -> Ge
  | LShift -> raise NotImplemented
  | RShift -> raise NotImplemented
  | RShift3 -> raise NotImplemented
  | Plus -> Plus
  | Minus -> Minus
  | Mult -> Times
  | Exp -> raise NotImplemented
  | Div -> raise NotImplemented
  | Mod -> raise NotImplemented
  | BitOr -> raise NotImplemented
  | Xor -> raise NotImplemented
  | BitAnd -> raise NotImplemented
  | In -> raise NotImplemented
  | Instanceof -> raise NotImplemented

type label = LBreak of string option | LContinue of string option
[@@deriving compare, equal]

type 'a flat_completion' = CNormal | CBreak of label | CReturn of 'a
[@@deriving compare, equal]

type flat_completion = unit flat_completion'

let compare_flat_completion = compare_flat_completion' compare_unit
let equal_flat_completion = equal_flat_completion' equal_unit

type 'a completion' =
  | CDet of 'a flat_completion'
  | CIndet of 'a flat_completion' list
[@@deriving compare, equal]

type completion = unit completion'

let compare_completion = compare_completion' compare_unit
let equal_completion = equal_completion' equal_unit
let cpl_to_flat_list = function CDet cpl -> [ cpl ] | CIndet cpls -> cpls

let loc_of_jsloc (loc : Loc.t) : Location.t =
  let file =
    match loc.source with Some v -> File_key.to_string v | None -> ""
  in
  let open Location in
  {
    loc_start =
      {
        pos_fname = file;
        pos_lnum = loc.start.line;
        pos_cnum = loc.start.column;
        pos_bol = 0;
      };
    loc_end =
      {
        pos_fname = file;
        pos_lnum = loc._end.line;
        pos_cnum = loc._end.column;
        pos_bol = 0;
      };
    loc_ghost = false;
  }

let string_of_label = function
  | LBreak None -> "brk"
  | LContinue None -> "con"
  | LBreak (Some s) -> "brk:" ^ s
  | LContinue (Some s) -> "con:" ^ s

let expr_desc_is_unit = function
  | Syntax.Expr.Const Syntax.Expr.Unit -> true
  | _ -> false

let expr_is_unit (Ex { desc; _ } : Syntax.Expr.some_expr) : bool =
  expr_desc_is_unit desc

let make_match ~(var : string) ?(base : Syntax.Expr.some_expr option) cases :
    Syntax.Expr.some_expr =
  let open Syntax.Expr in
  match (base, List.rev cases) with
  | None, [] -> x_const_unit ()
  | Some base, cases | None, (_, base) :: cases ->
      List.fold cases ~init:base ~f:(fun last_expr (cmp, expr) ->
          let pat =
            match cmp with
            | CNormal ->
                x_bop
                  {
                    op = Eq;
                    left = x_get { obj = x_var var; idx = x_const_string "tag" };
                    right = x_const_string "NRM";
                  }
            | CBreak label ->
                x_bop
                  {
                    op = And;
                    left =
                      x_bop
                        {
                          op = Eq;
                          left =
                            x_get
                              { obj = x_var var; idx = x_const_string "tag" };
                          right = x_const_string "BRK";
                        };
                    right =
                      x_bop
                        {
                          op = Eq;
                          left =
                            x_get
                              { obj = x_var var; idx = x_const_string "label" };
                          right = x_const_string (string_of_label label);
                        };
                  }
            | CReturn _ ->
                x_bop
                  {
                    op = Eq;
                    left = x_get { obj = x_var var; idx = x_const_string "tag" };
                    right = x_const_string "RET";
                  }
          in
          x_cond { pred = pat; con = expr; alt = last_expr })

let make_obj_expr ?(loc = Location.none)
    (pairs : (Syntax.Expr.some_expr * Syntax.Expr.some_expr) list) :
    Syntax.Expr.some_expr =
  let open Syntax.Expr in
  let obj = fresh_id () in
  let asgns =
    pairs
    |> List.map ~f:(fun (key, value) ->
           x_set { obj = x_var obj; idx = key; value })
  in
  let asgns =
    asgns |> List.rev
    |> List.fold ~init:(x_var obj) ~f:(fun last_expr asgn ->
           x_seq (asgn, last_expr))
  in
  x_let ~loc { id = obj; bound = x_alloc (); body = asgns }

let cpl_literal_expr e : Syntax.Expr.some_expr =
  let open Syntax.Expr in
  match e with
  | CNormal -> make_obj_expr [ (x_const_string "tag", x_const_string "NRM") ]
  | CBreak label ->
      make_obj_expr
        [
          (x_const_string "tag", x_const_string "BRK");
          (x_const_string "label", x_const_string (string_of_label label));
        ]
  | CReturn expr ->
      make_obj_expr
        [
          (x_const_string "tag", x_const_string "RET");
          (x_const_string "value", expr);
        ]

(* return a wrapped expression that always returns a completion object *)
let wrap_cpl (cpl : completion) (expr : Syntax.Expr.some_expr) :
    Syntax.Expr.some_expr =
  let open Syntax.Expr in
  match cpl with
  | CDet ((CNormal | CBreak _) as cpl) ->
      let cpl_literal = cpl_literal_expr cpl in
      if expr_is_unit expr then cpl_literal else x_seq (expr, cpl_literal)
  | CDet (CReturn _) -> cpl_literal_expr (CReturn expr)
  | CIndet _ -> expr

let make_seq ?(loc = Location.none)
    ((e1, cpl1) : Syntax.Expr.some_expr * completion)
    ((e2, cpl2) : Syntax.Expr.some_expr * completion) :
    Syntax.Expr.some_expr * completion =
  let open Syntax.Expr in
  match cpl1 with
  | CDet CNormal -> (
      (* try to reduce redundant unit expressions *)
      match (e1, e2, cpl2) with
      | Ex { desc = Const Unit; _ }, _, _ -> (e2, cpl2)
      | _, Ex { desc = Const Unit; _ }, CDet CNormal -> (e1, CDet CNormal)
      | _ -> (x_seq ~loc (e1, e2), cpl2))
  | CDet (CBreak _ | CReturn _) -> (e1, cpl1)
  | CIndet _ when expr_is_unit e2 && equal_completion cpl2 (CDet CNormal) ->
      (* should output (let cpl = e1 in if cpl.tag = "NRM" then { tag: "NRM" }
         else cpl), which is equivalent to (e1) *)
      (e1, cpl1)
  | CIndet cpls1
    when List.exists cpls1 ~f:(fun cpl -> equal_flat_completion cpl CNormal) ->
      (* let cpl = e1 in if cpl.tag = "NRM" then [wrapped] e2 else cpl *)
      let open Syntax.Expr in
      let cpls1' =
        List.filter cpls1 ~f:(fun cpl ->
            not (equal_flat_completion cpl CNormal))
      in
      let cpls2' = cpl_to_flat_list cpl2 in
      let cpl_var = fresh_id () in
      ( x_let ~loc
          {
            id = cpl_var;
            bound = e1;
            body =
              x_cond
                {
                  pred =
                    x_bop
                      {
                        op = Eq;
                        left =
                          x_get
                            { obj = x_var cpl_var; idx = x_const_string "tag" };
                        right = x_const_string "NRM";
                      };
                  con = wrap_cpl cpl2 e2;
                  alt = x_var cpl_var;
                };
          },
        CIndet (cpls1' @ cpls2') )
  | _ ->
      (* e2 is never executed *)
      (e1, cpl1)

let make_cond ?(loc = Location.none) (test : Syntax.Expr.some_expr)
    ((con, con_cpl) : Syntax.Expr.some_expr * completion)
    ((alt, alt_cpl) : Syntax.Expr.some_expr * completion) :
    Syntax.Expr.some_expr * completion =
  let open Syntax.Expr in
  match (con_cpl, alt_cpl) with
  | CDet con_cpl', CDet alt_cpl' when equal_flat_completion con_cpl' alt_cpl' ->
      (x_cond ~loc { pred = test; con; alt }, CDet con_cpl')
  | _, _ ->
      let con_cpl' = cpl_to_flat_list con_cpl in
      let alt_cpl' = cpl_to_flat_list alt_cpl in
      let cpl =
        List.dedup_and_sort ~compare:compare_flat_completion
          (con_cpl' @ alt_cpl')
      in
      ( x_cond ~loc
          {
            pred = test;
            con = wrap_cpl con_cpl con;
            alt = wrap_cpl alt_cpl alt;
          },
        CIndet cpl )

let make_repeat ?(loc = Location.none) label
    ((body, cpl) : Syntax.Expr.some_expr * completion) :
    Syntax.Expr.some_expr * completion =
  (* use my repeat desugar *)
  let open Syntax.Expr in
  match cpl_to_flat_list cpl with
  | [ CNormal ] ->
      (* (rec f = fun x -> body; f ()) () *)
      let func_name = fresh_id () in
      let param_name = fresh_id () in
      ( x_app ~loc
          {
            fn =
              x_fn
                {
                  self = Some func_name;
                  param = param_name;
                  body =
                    x_seq
                      ( body,
                        x_app { fn = x_var func_name; arg = x_const_unit () } );
                };
            arg = x_const_unit ();
          },
        CDet CNormal )
  | [ CBreak label' ] when equal_label label label' -> (body, CDet CNormal)
  | [ (CBreak _ | CReturn _) ] -> (body, cpl)
  | cpls ->
      (* CIndet _ *)
      let func_name = fresh_id () in
      let param_name = fresh_id () in
      let cpl_name = fresh_id () in
      let brk_cpl, nrm_cpl, other_cpls =
        List.partition3_map cpls ~f:(fun cpl ->
            match cpl with
            | CBreak label' when equal_label label label' -> `Fst ()
            | CNormal -> `Snd ()
            | _ -> `Trd cpl)
      in
      (* (fix f x. let c = <body> in match c with | { tag: "BRK", label: <label>
         } -> { tag: "NRM" } | { tag: "NRM" } -> f x | _ -> c) () *)
      let brk_case =
        if List.is_empty brk_cpl then []
        else [ (CBreak label, cpl_literal_expr CNormal) ]
      in
      let nrm_case =
        if List.is_empty nrm_cpl then []
        else
          [ (CNormal, x_app { fn = x_var func_name; arg = x_const_unit () }) ]
      in
      let base_case =
        if List.is_empty other_cpls then None else Some (x_var cpl_name)
      in
      let expr =
        x_app ~loc
          {
            fn =
              x_fn
                {
                  self = Some func_name;
                  param = param_name;
                  body =
                    x_let
                      {
                        id = cpl_name;
                        bound = body;
                        body =
                          make_match ~var:cpl_name ?base:base_case
                            (brk_case @ nrm_case);
                      };
                };
            arg = x_const_unit ();
          }
      in
      let cpls' =
        cpls
        |> List.filter_map ~f:(fun cpl ->
               match cpl with
               | CNormal -> None
               | CBreak label' when equal_label label label' -> Some CNormal
               | _ -> Some cpl)
        |> List.dedup_and_sort ~compare:compare_flat_completion
      in
      (expr, CIndet cpls')

let rec convert_stat_list ~loc (body : (Loc.t, Loc.t) Flow_ast.Statement.t list)
    : Syntax.Expr.some_expr * completion =
  let open Syntax.Expr in
  let nop_pair = (x_const_unit (), CDet CNormal) in
  let rec convert_stat_tail ~loc:whole_loc (tail, tail_cpl)
      ((loc, stmt) : (Loc.t, Loc.t) Flow_ast.Statement.t) =
    let res =
      match stmt with
      | Block { body; _ } ->
          let body, cpl = convert_stat_list ~loc body in
          make_seq ~loc:whole_loc (tail, tail_cpl) (body, cpl)
      | Break { label; _ } ->
          let label = Option.map label ~f:(fun (_, { name; _ }) -> name) in
          (x_const_unit ~loc:(loc_of_jsloc loc) (), CDet (CBreak (LBreak label)))
      | ClassDeclaration _ -> raise NotImplemented
      | ComponentDeclaration _ -> raise NotImplemented
      | Continue { label; _ } ->
          let label = Option.map label ~f:(fun (_, { name; _ }) -> name) in
          ( x_const_unit ~loc:(loc_of_jsloc loc) (),
            CDet (CBreak (LContinue label)) )
      | Debugger _ -> (tail, tail_cpl)
      | DeclareClass _ | DeclareComponent _ | DeclareEnum _
      | DeclareExportDeclaration _ | DeclareFunction _ | DeclareInterface _
      | DeclareModule _ | DeclareModuleExports _ | DeclareNamespace _
      | DeclareTypeAlias _ | DeclareOpaqueType _ | DeclareVariable _ ->
          (* flow statements starting with 'declare' *)
          (tail, tail_cpl)
      | DoWhile { body; test; _ } ->
          (* while and do while are symmetric *)
          make_seq ~loc:whole_loc
            (make_repeat ~loc:(loc_of_jsloc loc) (LBreak None)
               (make_repeat (LContinue None)
                  (make_seq (convert_stat body)
                     (make_cond (convert_expr test) nop_pair
                        (x_const_unit (), CDet (CBreak (LBreak None)))))))
            (tail, tail_cpl)
      | Empty _ -> (tail, tail_cpl)
      | EnumDeclaration _ -> raise NotImplemented
      | ExportDefaultDeclaration { declaration; _ } -> (
          (* TODO: handle export default declaration *)
          match declaration with
          | Declaration stmt ->
              (* delegate to var and function declaration *)
              convert_stat_tail ~loc:whole_loc (tail, tail_cpl) stmt
          | Expression expr ->
              (x_seq ~loc:whole_loc (convert_expr expr, tail), tail_cpl))
      | ExportNamedDeclaration { declaration; _ } -> (
          (* TODO: handle export named declaration, especially those without
             declaration *)
          match declaration with
          | Some stmt ->
              (* delegate to var and function declaration *)
              convert_stat_tail ~loc:whole_loc (tail, tail_cpl) stmt
          | None -> (tail, tail_cpl))
      | Expression
          {
            expression =
              ( _,
                Call
                  {
                    callee = _, Identifier (_, { name = "useEffect"; _ });
                    arguments =
                      ( _,
                        {
                          arguments =
                            [
                              Expression
                                ( _,
                                  ArrowFunction
                                    {
                                      params =
                                        ( _,
                                          {
                                            params = [];
                                            this_ = None;
                                            rest = None;
                                            _;
                                          } );
                                      body;
                                      _;
                                    } );
                            ];
                          _;
                        } );
                    _;
                  } );
            _;
          } ->
          (* useEffect(() => body); *)
          let body' =
            match body with
            | BodyBlock (body_loc, { body; _ }) ->
                fst (convert_stat_list ~loc:body_loc body)
            | BodyExpression expr -> convert_expr expr
          in
          ( x_seq ~loc:whole_loc (x_eff ~loc:(loc_of_jsloc loc) body', tail),
            tail_cpl )
      | Expression { expression; _ } ->
          let expr = convert_expr expression in
          make_seq ~loc:whole_loc (expr, CDet CNormal) (tail, tail_cpl)
      | For _ -> raise NotImplemented
      | ForIn _ -> raise NotImplemented
      | ForOf _ -> raise NotImplemented
      | FunctionDeclaration f -> (
          let expr = convert_func ~loc f in
          match f.id with
          | Some (_, { name; _ }) ->
              ( x_let ~loc:whole_loc { id = name; bound = expr; body = tail },
                tail_cpl )
          | None -> make_seq ~loc:whole_loc (expr, CDet CNormal) (tail, tail_cpl)
          )
      | If { test; consequent; alternate; _ } ->
          let test = convert_expr test in
          let con = convert_stat consequent in
          let alt =
            match alternate with
            | Some (_, { body; _ }) -> convert_stat body
            | None -> nop_pair
          in
          make_seq ~loc:whole_loc
            (make_cond ~loc:(loc_of_jsloc loc) test con alt)
            (tail, tail_cpl)
      | ImportDeclaration _ -> raise NotImplemented
      | InterfaceDeclaration _ -> raise NotImplemented
      | Labeled _ ->
          (* TODO: handle labeled statement *)
          (tail, tail_cpl)
      | Return { argument = Some expr; _ } ->
          let expr = convert_expr expr in
          (expr, CDet (CReturn ()))
      | Return { argument = None; _ } ->
          (x_const_unit ~loc:(loc_of_jsloc loc) (), CDet (CReturn ()))
      | Switch _ -> raise NotImplemented
      | Throw _ -> raise NotImplemented
      | Try _ -> raise NotImplemented
      | TypeAlias _ | OpaqueType _ ->
          (* flow type declaration *)
          (tail, tail_cpl)
      | VariableDeclaration
          {
            declarations =
              [
                ( _,
                  {
                    id =
                      ( _,
                        Array
                          {
                            elements =
                              [
                                Element
                                  ( _,
                                    {
                                      argument =
                                        ( _,
                                          Identifier
                                            {
                                              name = _, { name = var_state; _ };
                                              _;
                                            } );
                                      _;
                                    } );
                                Element
                                  ( _,
                                    {
                                      argument =
                                        ( _,
                                          Identifier
                                            {
                                              name = _, { name = var_setter; _ };
                                              _;
                                            } );
                                      _;
                                    } );
                              ];
                            _;
                          } );
                    init =
                      Some
                        ( _,
                          Call
                            {
                              callee =
                                _, Identifier (_, { name = "useState"; _ });
                              arguments =
                                _, { arguments = [ Expression init ]; _ };
                              _;
                            } );
                    _;
                  } );
              ];
            _;
          } ->
          (* let [var_state, var_setter] = useState(init) *)
          ( x_stt ~loc:whole_loc
              {
                label = fresh_label ();
                stt = var_state;
                set = var_setter;
                init = convert_expr init;
                body = tail;
              },
            tail_cpl )
      | VariableDeclaration { declarations; _ } ->
          let decls =
            List.concat_map declarations ~f:(fun (_, { id; init; _ }) ->
                let init =
                  match init with
                  | Some expr -> convert_expr expr
                  | None -> x_const_unit ()
                in
                convert_pattern id ~base_expr:init)
          in
          ( decls |> List.rev
            |> List.fold ~init:tail ~f:(fun tail (loc, name, expr) ->
                   x_let ~loc:(loc_of_jsloc loc)
                     { id = name; bound = expr; body = tail }),
            tail_cpl )
      | While { body; test; _ } ->
          (* while and do while are symmetric *)
          make_repeat ~loc:whole_loc (LBreak None)
            (make_repeat (LContinue None)
               (make_seq
                  (make_cond (convert_expr test) nop_pair
                     (x_const_unit (), CDet (CBreak (LBreak None))))
                  (convert_stat body)))
      | With _ -> raise NotImplemented
      | Match _ -> raise NotImplemented
    in
    (* print_endline ("convert_stat " ^ (Flow_ast.Statement.show_t' Loc.pp
       Loc.pp stmt) ^ " = \n" ^ (Syntax.Expr.sexp_of_hook_free_t (fst res) |>
       Sexp.to_string) ); *)
    res
  and convert_stat (stmt : (Loc.t, Loc.t) Flow_ast.Statement.t) :
      Syntax.Expr.some_expr * completion =
    let loc, _ = stmt in
    convert_stat_tail ~loc:(loc_of_jsloc loc) nop_pair stmt
  in
  let res =
    List.rev body
    |> List.foldi ~init:nop_pair ~f:(fun i tail stmt ->
           let loc = if i = 0 then loc_of_jsloc loc else Location.none in
           convert_stat_tail ~loc tail stmt)
  in
  (* print_endline ("convert_stat_list [" ^ (List.map body ~f:(fun (_, stmt) ->
     Flow_ast.Statement.show_t' Loc.pp Loc.pp stmt) |> String.concat ~sep:"; ")
     ^ "] = \n" ^ (Syntax.Expr.sexp_of_hook_free_t (fst res) |> Sexp.to_string)
     ); *)
  res

and convert_func ~loc (f : (Loc.t, Loc.t) Flow_ast.Function.t) :
    Syntax.Expr.some_expr =
  let self, param, body = convert_func_body f in
  Syntax.Expr.x_fn ~loc:(loc_of_jsloc loc) { self; param; body }

and convert_func_body
    ({ id; params; body; _ } : (Loc.t, Loc.t) Flow_ast.Function.t) :
    string option * string * Syntax.Expr.some_expr =
  let open Syntax.Expr in
  let self = Option.map id ~f:(fun (_, { name; _ }) -> name) in
  let param =
    match params with
    | _, { params = [ (_, { argument; default = None }) ]; _ } -> argument
    | _ -> raise NotImplemented (* non-single or optional parameter *)
  in
  let param_name, param_bindings =
    match param with
    | _, Identifier { name = _, { name; _ }; _ } -> (name, [])
    | _ ->
        let param_name = fresh_id () in
        let param_bindings =
          convert_pattern param ~base_expr:(x_var param_name)
        in
        (param_name, param_bindings)
  in
  let body, cpl =
    match body with
    | BodyBlock (body_loc, { body; _ }) -> convert_stat_list ~loc:body_loc body
    | BodyExpression expr -> (convert_expr expr, CDet (CReturn ()))
  in
  let body =
    List.rev param_bindings
    |> List.fold ~init:body ~f:(fun last_expr (loc, name, expr) ->
           x_let ~loc:(loc_of_jsloc loc)
             { id = name; bound = expr; body = last_expr })
  in
  match cpl_to_flat_list cpl with
  | [ CNormal ] ->
      let body', _ =
        make_seq (body, cpl) (x_const_unit (), CDet (CReturn ()))
      in
      (self, param_name, body')
  | [ (CBreak _ | CReturn _) ] -> (self, param_name, body)
  | _ ->
      (* CIndet _ *)
      (* λx. let r = body in if r.tag = "RET" then r.value else () *)
      let ret_var = fresh_id () in
      let body' =
        x_let
          {
            id = ret_var;
            bound = body;
            body =
              make_match ~var:ret_var
                [
                  ( CReturn
                      (x_get
                         { obj = x_var ret_var; idx = x_const_string "value" }),
                    x_get { obj = x_var ret_var; idx = x_const_string "value" }
                  );
                ]
                ~base:(x_const_unit ());
          }
      in
      (self, param_name, body')

and convert_call ~loc:whole_loc (callee : Syntax.Expr.some_expr)
    ((_, { arguments; _ }) : (Loc.t, Loc.t) Flow_ast.Expression.ArgList.t) :
    Syntax.Expr.some_expr =
  let open Syntax.Expr in
  let argument =
    match arguments with
    | [ Expression expr ] -> convert_expr expr
    | _ -> raise NotImplemented (* non-single or spread arguments *)
  in
  x_app ~loc:(loc_of_jsloc whole_loc) { fn = callee; arg = argument }

and convert_member ?(loc = Location.none) (obj : Syntax.Expr.some_expr)
    (property : (Loc.t, Loc.t) Flow_ast.Expression.Member.property) :
    Syntax.Expr.some_expr =
  let open Syntax.Expr in
  match property with
  | PropertyIdentifier (name_loc, { name; _ }) ->
      x_get ~loc { obj; idx = x_const_string ~loc:(loc_of_jsloc name_loc) name }
  | PropertyPrivateName _ -> raise NotImplemented
  | PropertyExpression expr -> x_get ~loc { obj; idx = convert_expr expr }

and convert_expr ((loc, expr) : (Loc.t, Loc.t) Flow_ast.Expression.t) :
    Syntax.Expr.some_expr =
  let open Syntax.Expr in
  match expr with
  | Array { elements; _ } ->
      (* [e0, e1] -> (let arr = {} in arr[0] := e0; arr[1] := e1; arr) *)
      make_obj_expr ~loc:(loc_of_jsloc loc)
        (List.mapi elements ~f:(fun i element ->
             match element with
             | Expression expr ->
                 (x_const_string (Int.to_string i), convert_expr expr)
             | _ -> raise NotImplemented))
  | Function f | ArrowFunction f -> convert_func ~loc f
  | AsConstExpression { expression; _ } -> convert_expr expression
  | AsExpression { expression; _ } -> convert_expr expression
  | Assignment _ -> raise NotImplemented
  | Binary { operator; left; right; _ } ->
      let left = convert_expr left in
      let right = convert_expr right in
      x_bop ~loc:(loc_of_jsloc loc) { op = convert_bop operator; left; right }
  | Call { callee; arguments; _ } ->
      let callee = convert_expr callee in
      convert_call ~loc callee arguments
  | Class _ -> raise NotImplemented
  | Conditional { test; consequent; alternate; _ } ->
      let test = convert_expr test in
      let consequent = convert_expr consequent in
      let alternate = convert_expr alternate in
      x_cond ~loc:(loc_of_jsloc loc)
        { pred = test; con = consequent; alt = alternate }
  | Identifier (_, { name; _ }) -> x_var ~loc:(loc_of_jsloc loc) name
  | Import _ -> raise NotImplemented
  | JSXElement { opening_element = _, { name; _ }; _ } ->
      (* TODO: handle opening and attributes and children *)
      let name =
        match name with
        | Identifier (var_loc, { name; _ }) ->
            x_var ~loc:(loc_of_jsloc var_loc) name
        | MemberExpression (mem_loc, name) ->
            let open Flow_ast.JSX.MemberExpression in
            let rec loop ~loc { _object; property = _, { name; _ }; _ } =
              let obj =
                match _object with
                | Identifier (var_loc, { name; _ }) ->
                    x_var ~loc:(loc_of_jsloc var_loc) name
                | MemberExpression (mem_loc, obj) ->
                    loop ~loc:(loc_of_jsloc mem_loc) obj
              in
              x_get ~loc { obj; idx = x_const_string name }
            in
            loop ~loc:(loc_of_jsloc mem_loc) name
        | _ -> raise NotImplemented (* non-identifier JSX element name *)
      in
      x_view ~loc:(loc_of_jsloc loc)
        [ x_app { fn = name; arg = x_const_unit () } ]
  | JSXFragment _ ->
      (* TODO *)
      x_view ~loc:(loc_of_jsloc loc) [ x_const_unit () ]
  | StringLiteral { value; _ } -> x_const_string ~loc:(loc_of_jsloc loc) value
  | BooleanLiteral { value; _ } -> x_const_bool ~loc:(loc_of_jsloc loc) value
  | NullLiteral _ ->
      (* TODO: discriminate null and undefined *)
      x_const_unit ~loc:(loc_of_jsloc loc) ()
  | NumberLiteral { value; _ } ->
      (* TODO: handle non-int value *)
      x_const_int ~loc:(loc_of_jsloc loc) (Int.of_float value)
  | BigIntLiteral { value = Some value; _ } ->
      x_const_int ~loc:(loc_of_jsloc loc) (Int64.to_int_exn value)
  | BigIntLiteral { value = None; _ } -> raise NotImplemented
  | RegExpLiteral _ -> raise NotImplemented
  | ModuleRefLiteral _ -> raise NotImplemented
  | Logical { operator; left; right; _ } -> (
      let left = convert_expr left in
      let right = convert_expr right in
      match operator with
      | Or ->
          (* a || b --> let a' = a in (if a' then a' else b) *)
          let name = fresh_id () in
          x_let ~loc:(loc_of_jsloc loc)
            {
              id = name;
              bound = left;
              body = x_cond { pred = x_var name; con = x_var name; alt = right };
            }
      | And ->
          (* a && b --> let a' = a in (if a' then b else a') *)
          let name = fresh_id () in
          x_let ~loc:(loc_of_jsloc loc)
            {
              id = name;
              bound = left;
              body = x_cond { pred = x_var name; con = right; alt = x_var name };
            }
      | NullishCoalesce ->
          (* a ?? b --> let a' = a in (if a' = () then b else a') *)
          let name = fresh_id () in
          x_let ~loc:(loc_of_jsloc loc)
            {
              id = name;
              bound = left;
              body =
                x_cond
                  {
                    pred =
                      x_bop
                        { op = Eq; left = x_var name; right = x_const_unit () };
                    con = right;
                    alt = x_var name;
                  };
            })
  | Member { _object; property; _ } ->
      let obj = convert_expr _object in
      convert_member ~loc:(loc_of_jsloc loc) obj property
  | MetaProperty _ -> raise NotImplemented
  | New _ -> raise NotImplemented
  | Object { properties; _ } ->
      let convert_key
          (key : (Loc.t, Loc.t) Flow_ast.Expression.Object.Property.key) =
        match key with
        | StringLiteral (key_loc, { value; _ }) ->
            x_const_string ~loc:(loc_of_jsloc key_loc) value
        | NumberLiteral (key_loc, { value; _ }) ->
            x_const_int ~loc:(loc_of_jsloc key_loc) (Int.of_float value)
        | BigIntLiteral (key_loc, { value = Some value; _ }) ->
            x_const_int ~loc:(loc_of_jsloc key_loc) (Int64.to_int_exn value)
        | BigIntLiteral (_, { value = None; _ }) -> raise NotImplemented
        | Identifier (key_loc, { name; _ }) ->
            x_const_string ~loc:(loc_of_jsloc key_loc) name
        | PrivateName _ -> raise NotImplemented
        | Computed (_, { expression; _ }) -> convert_expr expression
      in
      make_obj_expr ~loc:(loc_of_jsloc loc)
        (List.map properties ~f:(function
          | Property (_, Init { key; value; _ }) ->
              (convert_key key, convert_expr value)
          | Property (_, Method { key; value = func_loc, value; _ }) ->
              (convert_key key, convert_func ~loc:func_loc value)
          | Property (_, Get _) -> raise NotImplemented
          | Property (_, Set _) -> raise NotImplemented
          | SpreadProperty _ -> raise NotImplemented))
  | OptionalCall { optional; call = { callee; arguments; _ }; _ } ->
      (* f?.(x) --> let f' = f in (if f' = () then () else (f' x)) *)
      let callee = convert_expr callee in
      let name = fresh_id () in
      if optional then
        x_let ~loc:(loc_of_jsloc loc)
          {
            id = name;
            bound = callee;
            body =
              x_cond
                {
                  pred =
                    x_bop
                      { op = Eq; left = x_var name; right = x_const_unit () };
                  con = x_const_unit ();
                  alt = convert_call ~loc (x_var name) arguments;
                };
          }
      else convert_call ~loc callee arguments
  | OptionalMember { optional; member = { _object; property; _ }; _ } ->
      (* obj?.x --> let obj' = obj in (if obj' = () then () else obj'.x) *)
      let obj = convert_expr _object in
      if optional then
        let name = fresh_id () in
        x_let ~loc:(loc_of_jsloc loc)
          {
            id = name;
            bound = obj;
            body =
              x_cond
                {
                  pred =
                    x_bop
                      { op = Eq; left = x_var name; right = x_const_unit () };
                  con = x_const_unit ();
                  alt = convert_member (x_var name) property;
                };
          }
      else convert_member ~loc:(loc_of_jsloc loc) obj property
  | Sequence { expressions; _ } ->
      List.fold expressions ~init:(x_const_unit ()) ~f:(fun left right ->
          x_seq (left, convert_expr right))
  | Super _ -> raise NotImplemented
  | TaggedTemplate _ -> raise NotImplemented
  | TemplateLiteral _ -> raise NotImplemented
  | This _ -> raise NotImplemented
  | TypeCast { expression; _ } -> convert_expr expression
  | TSSatisfies { expression; _ } -> convert_expr expression
  | Unary { operator; argument; _ } -> (
      let argument = convert_expr argument in
      let open Syntax.Expr in
      match operator with
      | Minus -> x_uop ~loc:(loc_of_jsloc loc) { op = Uminus; arg = argument }
      | Plus -> x_uop ~loc:(loc_of_jsloc loc) { op = Uplus; arg = argument }
      | Not -> x_uop ~loc:(loc_of_jsloc loc) { op = Not; arg = argument }
      | BitNot -> raise NotImplemented
      | Typeof -> raise NotImplemented
      | Void -> x_seq ~loc:(loc_of_jsloc loc) (argument, x_const_unit ())
      | Delete -> raise NotImplemented
      | Await -> raise NotImplemented)
  | Update _ -> raise NotImplemented
  | Yield _ -> raise NotImplemented
  | Match _ -> raise NotImplemented

and convert_pattern ((loc, pattern) : (Loc.t, Loc.t) Flow_ast.Pattern.t)
    ~(base_expr : Syntax.Expr.some_expr) :
    (Loc.t * string * Syntax.Expr.some_expr) list =
  let open Syntax.Expr in
  match pattern with
  | Identifier { name = _, { name; _ }; _ } -> [ (loc, name, base_expr) ]
  | Object { properties; _ } ->
      let base_name = fresh_id () in
      (loc, base_name, base_expr)
      :: List.concat_map properties ~f:(function
           | Property (_, { key; pattern; default = None; _ }) ->
               let key =
                 match key with
                 | StringLiteral (key_loc, { value; _ }) ->
                     x_const_string ~loc:(loc_of_jsloc key_loc) value
                 | NumberLiteral (key_loc, { value; _ }) ->
                     x_const_int ~loc:(loc_of_jsloc key_loc)
                       (Int.of_float value)
                 | BigIntLiteral (key_loc, { value = Some value; _ }) ->
                     x_const_int ~loc:(loc_of_jsloc key_loc)
                       (Int64.to_int_exn value)
                 | BigIntLiteral (_, { value = None; _ }) ->
                     raise NotImplemented
                 | Identifier (key_loc, { name; _ }) ->
                     x_const_string ~loc:(loc_of_jsloc key_loc) name
                 | Computed (_, { expression; _ }) -> convert_expr expression
               in
               convert_pattern pattern
                 ~base_expr:(x_get { obj = x_var base_name; idx = key })
           | Property (_, { default = Some _; _ }) -> raise NotImplemented
           | RestElement _ -> raise NotImplemented)
  | Array { elements; _ } ->
      let base_name = fresh_id () in
      (loc, base_name, base_expr)
      :: List.concat_mapi elements ~f:(fun i -> function
           | Element (_, { argument; default = None }) ->
               convert_pattern argument
                 ~base_expr:
                   (x_get { obj = x_var base_name; idx = x_const_int i })
           | Element (_, { default = Some _; _ }) -> raise NotImplemented
           | RestElement _ -> raise NotImplemented
           | Hole _ -> [])
  | Expression _ -> raise Unreachable

let map_while ~(f : 'a -> 'b option) (lst : 'a list) : 'b list * 'a list =
  let rec loop acc rest =
    match rest with
    | [] -> (List.rev acc, [])
    | hd :: tl -> (
        match f hd with
        | Some hd' -> loop (hd' :: acc) tl
        | None -> (List.rev acc, rest))
  in
  loop [] lst

let convert (js_ast : js_ast) : Syntax.Prog.t =
  let loc, { Flow_ast.Program.statements; _ } = js_ast in
  let open Syntax.Expr in
  let comps, stats =
    map_while statements
      ~f:(fun (_, (stmt : (Loc.t, Loc.t) Flow_ast.Statement.t')) ->
        match stmt with
        | VariableDeclaration
            {
              declarations =
                [
                  ( _,
                    {
                      id = _, Identifier { name = _, { name; _ }; _ };
                      init =
                        Some
                          ( _,
                            ( Function
                                ({
                                   id = None;
                                   params =
                                     ( _,
                                       {
                                         params = [ _ ];
                                         this_ = None;
                                         rest = None;
                                         _;
                                       } );
                                   _;
                                 } as f)
                            | ArrowFunction
                                ({
                                   id = None;
                                   params =
                                     ( _,
                                       {
                                         params = [ _ ];
                                         this_ = None;
                                         rest = None;
                                         _;
                                       } );
                                   _;
                                 } as f) ) );
                      _;
                    } );
                ];
              _;
            }
        | FunctionDeclaration
            ({
               id = Some (_, { name; _ });
               params = _, { params = [ _ ]; this_ = None; rest = None; _ };
               _;
             } as f)
          when String.get name 0 |> Char.is_uppercase ->
            (* component function candidate : function name starts with
               uppercase, single parameter *)
            let _, param, body = convert_func_body f in
            Some Syntax.Prog.{ name; param; body = hook_full body }
        | _ -> None)
  in
  let last_expr, _ = convert_stat_list ~loc stats in
  List.rev comps
  |> List.fold
       ~init:(Syntax.Prog.Expr (Syntax.Expr.hook_free_exn last_expr))
       ~f:(fun last_expr comp -> Syntax.Prog.Comp (comp, last_expr))
