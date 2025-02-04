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

(* placeholder for ~loc *)
let loc = Location.none

let fresh =
  let counter = ref 0 in
  fun () ->
    let n = !counter in
    Int.incr counter;
    "@@" ^ Int.to_string n

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

(*
type completion = CNormal | CBreak of label | CReturn | CIndet
[@@deriving compare, equal]
*)
type flat_completion = CNormal | CBreak of label | CReturn
[@@deriving compare, equal]
type completion = CDet of flat_completion | CIndet of flat_completion list
[@@deriving compare, equal]

let merge_completion (cpl1 : completion) (cpl2 : completion) : completion =
  let cpls =
    match (cpl1, cpl2) with
    | CDet cpl1, CDet cpl2 -> [ cpl1; cpl2 ]
    | CDet cpl1, CIndet cpl2 -> cpl1 :: cpl2
    | CIndet cpl1, CDet cpl2 -> cpl2 :: cpl1
    | CIndet cpl1, CIndet cpl2 -> cpl1 @ cpl2
  in
  let cpls = List.dedup_and_sort ~compare:compare_flat_completion cpls in
  match cpls with
  | [] -> CIndet []
  | [ cpl ] -> CDet cpl
  | _ -> CIndet cpls

let string_of_label = function
  | LBreak None -> "brk"
  | LContinue None -> "con"
  | LBreak (Some s) -> "brk:" ^ s
  | LContinue (Some s) -> "con:" ^ s

let expr_desc_is_unit = function
  | Syntax.Expr.Const Syntax.Expr.Unit -> true
  | _ -> false

let expr_is_unit ({ desc; _ } : Syntax.Expr.hook_free_t) : bool =
  expr_desc_is_unit desc

let cpl_literal_expr e =
  let open Syntax.Expr in
  let e' = match e with
  | (CNormal, None) ->
      let obj_var = fresh () in
      Let
            {
              id = obj_var;
              bound = mk ~loc (Alloc);
              body =
                mk ~loc (Seq
                  ( mk ~loc (Set
                      {
                        obj = mk ~loc (Var obj_var);
                        idx = mk ~loc (Const (String "tag"));
                        value = mk ~loc (Const (String "NRM"));
                      }),
                    mk ~loc (Var obj_var )));
            }
  | (CBreak label, None) ->
      let obj_var = fresh () in
          Let
            {
              id = obj_var;
              bound = mk ~loc (Alloc);
              body =
                mk ~loc (Seq
                  ( mk ~loc (Set
                      {
                        obj = mk ~loc (Var obj_var);
                        idx = mk ~loc (Const (String "tag"));
                        value = mk ~loc (Const (String "BRK"));
                      }),
                    mk ~loc (Seq
                      ( mk ~loc (Set
                          {
                            obj = mk ~loc (Var obj_var);
                            idx = mk ~loc (Const (String "label"));
                            value = mk ~loc (Const (String (string_of_label label)));
                          }),
                        mk ~loc (Var obj_var ) ))) );
            }
  | (CReturn, Some expr) ->
      let obj_var = fresh () in
            Let
              {
                id = obj_var;
                bound = mk ~loc (Alloc);
                body =
                  mk ~loc (Seq
                    ( mk ~loc (Set
                        {
                          obj = mk ~loc (Var obj_var);
                          idx = mk ~loc (Const (String "tag"));
                          value = mk ~loc (Const (String "RET"));
                        }),
                      mk ~loc (Seq
                        ( mk ~loc (Set
                            {
                              obj = mk ~loc (Var obj_var);
                              idx = mk ~loc (Const (String "value"));
                              value = expr
                            }),
                          mk ~loc (Var obj_var ) ))) );
              }
  | _ -> raise (Invalid_argument "cpl_literal_expr : expr should be present iff CReturn is used")
  in
  mk ~loc e'

(* return a wrapped expression that always returns a completion object *)
let wrap_cpl (cpl : completion) (expr : Syntax.Expr.hook_free_t) :
    Syntax.Expr.hook_free_t =
  let open Syntax.Expr in
  match cpl with
  | CDet ((CNormal | CBreak _) as cpl) ->
      let cpl_literal = cpl_literal_expr (cpl, None) in
      if expr_is_unit expr then cpl_literal else mk ~loc (Seq ( expr, cpl_literal ))
  | CDet CReturn ->
      cpl_literal_expr (CReturn, Some expr)
  | CIndet _ -> expr

let convert_seq ((e1, cpl1) : Syntax.Expr.hook_free_t * completion)
    ((e2, cpl2) : Syntax.Expr.hook_free_t * completion) :
    Syntax.Expr.hook_free_t * completion =
  let open Syntax.Expr in
  match cpl1 with
  | CDet CNormal ->
      (* try to reduce redundant unit expressions *)
      begin match e1, e2, cpl2 with
      | { desc = Const Unit; _ }, _, _ -> (e2, cpl2)
      | _, { desc = Const Unit; _ }, CDet CNormal -> (e1, CDet CNormal)
      | _ -> (mk ~loc (Seq (e1, e2)), cpl2)
      end
  | CDet (CBreak _ | CReturn) -> (e1, cpl1)
  | CIndet _ when (expr_is_unit e2 && equal_completion cpl2 (CDet CNormal)) ->
      (* should output (let cpl = e1 in if cpl.tag = "NRM" then { tag: "NRM" } else cpl),
         which is equivalent to (e1) *)
      (e1, cpl1)
  | CIndet cpls1 when List.exists cpls1 ~f:(fun cpl -> equal_flat_completion cpl CNormal) ->
      (* let cpl = e1 in if cpl.tag = "NRM" then [wrapped] e2 else cpl *)
      let open Syntax.Expr in
      let cpl_var = fresh () in
      ( mk ~loc (Let
          {
            id = cpl_var;
            bound = e1;
            body =
              mk ~loc (Cond
                {
                  pred =
                    mk ~loc (Bop
                      {
                        op = Eq;
                        left =
                          mk ~loc (Get { obj = mk ~loc (Var cpl_var); idx = mk ~loc (Const (String "tag")) });
                        right = mk ~loc (Const (String "NRM"));
                      });
                  con = wrap_cpl cpl2 e2;
                  alt = mk ~loc (Var cpl_var);
                });
          }),
        merge_completion cpl1 cpl2 )
  | CIndet _ ->
      (* e2 is never executed *)
      (e1, cpl1)

let convert_cond
  (test : Syntax.Expr.hook_free_t)
  ((con, con_cpl) : Syntax.Expr.hook_free_t * completion)
  ((alt, alt_cpl) : Syntax.Expr.hook_free_t * completion) :
    Syntax.Expr.hook_free_t * completion =
  let open Syntax.Expr in
  let cpl = merge_completion con_cpl alt_cpl in
  let (con, alt) =
    match cpl with
    | CDet _ -> (con, alt)
    | CIndet _ -> (wrap_cpl con_cpl con, wrap_cpl alt_cpl alt)
  in
  (mk ~loc (Cond { pred = test; con; alt }), cpl)

let convert_repeat label ((body, cpl) : Syntax.Expr.hook_free_t * completion) :
    Syntax.Expr.hook_free_t * completion =
  (* use my repeat desugar *)
  let open Syntax.Expr in
  match cpl with
  | CDet CNormal ->
      let func_name = fresh () in
      let param_name = fresh () in
      (
        mk ~loc (App {
          fn = mk ~loc (Fn {
            self = Some func_name;
            param = param_name;
            body = mk ~loc (Seq (
              body,
              mk ~loc (App {
                fn = mk ~loc (Var func_name);
                arg = mk ~loc (Const Unit)
              })
            ))
          });
          arg = mk ~loc (Const Unit)
        })
        ,
        CDet CNormal (* TODO: replace with (CIndet []) ? *)
      )
  | CDet (CBreak label') when equal_label label label' ->
      (body, CDet CNormal)
  | CDet (CBreak _ | CReturn) ->
      (body, cpl)
  | CIndet cpls ->
      let cpls' = cpls |> List.filter_map ~f:(fun cpl ->
        match cpl with
        | CNormal -> None
        | (CBreak label') when equal_label label label' -> Some CNormal
        | _ -> Some cpl
      ) |> List.dedup_and_sort ~compare:compare_flat_completion
      in
      let func_name = fresh () in
      let param_name = fresh () in
      let cpl_name = fresh () in
      (* (fix f x.
          let c = <body> in
          if c.tag = "BRK" && c.label = <label> then
            { tag: "NRM" }
          else if c.tag = "NRM" then
            f x
          else
            c) () *)
      (
        mk ~loc (App {
          fn = mk ~loc (Fn {
            self = Some func_name;
            param = param_name;
            body =
              mk ~loc (Let {
                id = cpl_name;
                bound = body;
                body =
                  mk ~loc (Cond {
                    pred = mk ~loc (Bop {
                      op = And;
                      left = mk ~loc (Bop {
                        op = Eq;
                        left = mk ~loc (Get { obj = mk ~loc (Var cpl_name); idx = mk ~loc (Const (String "tag") ) });
                        right = mk ~loc (Const (String "BRK"));
                      });
                      right = mk ~loc (Bop {
                        op = Eq;
                        left = mk ~loc (Get { obj = mk ~loc (Var cpl_name); idx = mk ~loc (Const (String "label") ) });
                        right = mk ~loc (Const (String (string_of_label label)))
                      });
                    });
                    con = cpl_literal_expr (CNormal, None);
                    alt =
                      mk ~loc (Cond {
                        pred = mk ~loc (Bop {
                          op = Eq;
                          left = mk ~loc (Get { obj = mk ~loc (Var cpl_name); idx = mk ~loc (Const (String "tag") ) });
                          right = mk ~loc (Const (String "NRM"));
                        });
                        con = mk ~loc (App {
                          fn = mk ~loc (Var func_name);
                          arg = mk ~loc (Const Unit);
                        });
                        alt = mk ~loc (Var cpl_name)
                      })
                  })
              })
          });
          arg = mk ~loc (Const Unit)
        })
        ,
        CIndet cpls'
      )

let rec convert_stat_list (body : (Loc.t, Loc.t) Flow_ast.Statement.t list) :
    Syntax.Expr.hook_free_t * completion =
  let open Syntax.Expr in
  let nop_pair = (mk ~loc (Const Unit), CDet CNormal) in
  let rec convert_stat_tail (tail, tail_cpl)
      ((_, stmt) : (Loc.t, Loc.t) Flow_ast.Statement.t) =
    let res = match stmt with
    | Block { body; _ } ->
        let body, cpl = convert_stat_list body in
        convert_seq (tail, tail_cpl) (body, cpl)
    | Break { label; _ } ->
        let label = Option.map label ~f:(fun (_, { name; _ }) -> name) in
        (mk ~loc (Const Unit), CDet (CBreak (LBreak label)))
    | ClassDeclaration _ -> raise NotImplemented
    | ComponentDeclaration _ -> raise NotImplemented
    | Continue { label; _ } ->
        let label = Option.map label ~f:(fun (_, { name; _ }) -> name) in
        (mk ~loc (Const Unit), CDet (CBreak (LContinue label)))
    | Debugger _ -> (tail, tail_cpl)
    | DeclareClass _ | DeclareComponent _ | DeclareEnum _
    | DeclareExportDeclaration _ | DeclareFunction _ | DeclareInterface _
    | DeclareModule _ | DeclareModuleExports _ | DeclareNamespace _
    | DeclareTypeAlias _ | DeclareOpaqueType _ | DeclareVariable _ ->
        (* flow statements starting with 'declare' *)
        (tail, tail_cpl)
    | DoWhile { body; test; _ } ->
        (* while and do while are symmetric *)
        convert_repeat (LBreak None) (
          convert_repeat (LContinue None) (
            convert_seq
            (convert_stat body)
            (convert_cond
              (convert_expr test)
              nop_pair
              (mk ~loc (Const Unit), CDet (CBreak (LBreak None))))))
    | Empty _ -> (tail, tail_cpl)
    | EnumDeclaration _ -> raise NotImplemented
    | ExportDefaultDeclaration { declaration; _ } -> (
        (* TODO: handle export default declaration *)
        match declaration with
        | Declaration stmt ->
            (* delegate to var and function declaration *)
            convert_stat_tail (tail, tail_cpl) stmt
        | Expression expr ->
            convert_seq (convert_expr expr, CDet CNormal) (tail, tail_cpl))
    | ExportNamedDeclaration { declaration; _ } -> (
        (* TODO: handle export named declaration, especially those without
           declaration *)
        match declaration with
        | Some stmt ->
            (* delegate to var and function declaration *)
            convert_stat_tail (tail, tail_cpl) stmt
        | None -> (tail, tail_cpl))
    | Expression { expression; _ } ->
        let expr = convert_expr expression in
        convert_seq (expr, CDet CNormal) (tail, tail_cpl)
    | For _ -> raise NotImplemented
    | ForIn _ -> raise NotImplemented
    | ForOf _ -> raise NotImplemented
    | FunctionDeclaration f -> (
        let expr = convert_func f in
        match f.id with
        | Some (_, { name; _ }) ->
            (mk ~loc (Let { id = name; bound = expr; body = tail }), tail_cpl)
        | None ->
            convert_seq (expr, CDet CNormal) (tail, tail_cpl))
    | If { test; consequent; alternate; _ } ->
        let test = convert_expr test in
        let con = convert_stat consequent in
        let alt = match alternate with
          | Some (_, { body; _ }) -> convert_stat body
          | None -> nop_pair
        in
        convert_seq (convert_cond test con alt) (tail, tail_cpl)
    | ImportDeclaration _ -> raise NotImplemented
    | InterfaceDeclaration _ -> raise NotImplemented
    | Labeled _ ->
        (* TODO: handle labeled statement *)
        (tail, tail_cpl)
    | Return { argument = Some expr; _ } ->
        let expr = convert_expr expr in
        (expr, CDet CReturn)
    | Return { argument = None; _ } -> (mk ~loc (Const Unit), CDet CReturn)
    | Switch _ -> raise NotImplemented
    | Throw _ -> raise NotImplemented
    | Try _ -> raise NotImplemented
    | TypeAlias _ | OpaqueType _ ->
        (* flow type declaration *)
        (tail, tail_cpl)
    | VariableDeclaration { declarations; _ } ->
        let decls =
          List.concat_map declarations ~f:(fun (_, { id; init; _ }) ->
              let init =
                match init with
                | Some expr -> convert_expr expr
                | None -> mk ~loc (Const Unit)
              in
              convert_pattern id ~base_expr:init)
        in
        ( decls |> List.rev
          |> List.fold ~init:tail ~f:(fun tail (name, expr) ->
                 mk ~loc (Let { id = name; bound = expr; body = tail })),
          tail_cpl )
    | While { body; test; _ } ->
        (* while and do while are symmetric *)
        convert_repeat (LBreak None) (
          convert_repeat (LContinue None) (
            convert_seq
            (convert_cond
              (convert_expr test)
              nop_pair
              (mk ~loc (Const Unit), CDet (CBreak (LBreak None))))
            (convert_stat body)))
    | With _ -> raise NotImplemented
    | Match _ -> raise NotImplemented
    in
    (*
    print_endline ("convert_stat " ^ (Flow_ast.Statement.show_t' Loc.pp Loc.pp stmt) ^ " = \n" ^
      (Syntax.Expr.sexp_of_hook_free_t (fst res) |> Sexp.to_string)
    );
    *)
    res
  and convert_stat (stmt : (Loc.t, Loc.t) Flow_ast.Statement.t) : Syntax.Expr.hook_free_t * completion =
    convert_stat_tail nop_pair stmt
  in
  let res = List.rev body |> List.fold ~init:nop_pair ~f:convert_stat_tail in
  (*
  print_endline ("convert_stat_list [" ^ (List.map body ~f:(fun (_, stmt) ->
    Flow_ast.Statement.show_t' Loc.pp Loc.pp stmt) |> String.concat ~sep:"; ") ^ "] = \n" ^
    (Syntax.Expr.sexp_of_hook_free_t (fst res) |> Sexp.to_string)
  );
  *)
  res

and convert_func ({ id; params; body; _ } : (Loc.t, Loc.t) Flow_ast.Function.t) :
    Syntax.Expr.hook_free_t =
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
        let param_name = fresh () in
        let param_bindings =
          convert_pattern param ~base_expr:(mk ~loc (Var param_name))
        in
        (param_name, param_bindings)
  in
  let body, cpl =
    match body with
    | BodyBlock (_, { body; _ }) -> convert_stat_list body
    | BodyExpression expr -> (convert_expr expr, CDet CNormal)
  in
  let body =
    List.rev param_bindings
    |> List.fold ~init:body ~f:(fun last_expr (name, expr) ->
           mk ~loc (Let { id = name; bound = expr; body = last_expr }))
  in
  match cpl with
  | CDet CNormal ->
      (* TODO: is this correct? *)
      let (body', _) = convert_seq (body, cpl) (mk ~loc (Const Unit), CDet CReturn) in
      mk ~loc (Fn {
    self;
    param = param_name; body = body' })
  | CDet (CBreak _ | CReturn) -> mk ~loc (Fn { self; param = param_name; body })
  | CIndet cpls ->
      (* λx. let r = body in if r.tag = "RET" then r.value else () *)
      let ret_var = fresh () in
      if List.exists cpls ~f:(fun cpl -> equal_flat_completion cpl CNormal) then
      mk ~loc (Fn
        {
          self;
          param = param_name;
          body =
            mk ~loc (Let
              {
                id = ret_var;
                bound = body;
                body =
                  mk ~loc (Cond
                    {
                      pred =
                        mk ~loc (Bop
                          {
                            op = Eq;
                            left =
                              mk ~loc (Get
                                {
                                  obj = mk ~loc (Var ret_var);
                                  idx = mk ~loc (Const (String "tag"));
                                });
                            right = mk ~loc (Const (String "RET"));
                          });
                      con =
                        mk ~loc (Get { obj = mk ~loc (Var ret_var); idx = mk ~loc (Const (String "value") ) });
                      alt = mk ~loc (Const Unit);
                    });
              });
        })
      else
        (* TODO: is this correct? *)
        mk ~loc (Fn { self; param = param_name; body })

and convert_call (callee : Syntax.Expr.hook_free_t)
    ((_, { arguments; _ }) : (Loc.t, Loc.t) Flow_ast.Expression.ArgList.t) :
    Syntax.Expr.hook_free_t =
  let open Syntax.Expr in
  let argument =
    match arguments with
    | [ Expression expr ] -> convert_expr expr
    | _ -> raise NotImplemented (* non-single or spread arguments *)
  in
  mk ~loc (App { fn = callee; arg = argument })

and convert_member (obj : Syntax.Expr.hook_free_t)
    (property : (Loc.t, Loc.t) Flow_ast.Expression.Member.property) :
    Syntax.Expr.hook_free_t =
  let open Syntax.Expr in
  match property with
  | PropertyIdentifier (_, { name; _ }) ->
      mk ~loc (Get { obj; idx = mk ~loc (Const (String name)) })
  | PropertyPrivateName _ -> raise NotImplemented
  | PropertyExpression expr -> mk ~loc (Get { obj; idx = convert_expr expr })

and convert_expr ((_, expr) : (Loc.t, Loc.t) Flow_ast.Expression.t) :
    Syntax.Expr.hook_free_t =
  let open Syntax.Expr in
  match expr with
  | Array { elements; _ } ->
      (* [e0, e1] -> (let arr = {} in arr[0] := e0; arr[1] := e1; arr) *)
      let arr = fresh () in
      let asgns =
        List.mapi elements ~f:(fun i element ->
            match element with
            | Expression expr ->
                let elem = convert_expr expr in
                mk ~loc (Set { obj = mk ~loc (Var arr); idx = mk ~loc (Const (Int i)); value = elem })
            | _ -> raise NotImplemented)
      in
      let asgns =
        asgns |> List.rev
        |> List.fold ~init:(mk ~loc (Var arr)) ~f:(fun last_expr asgn ->
               mk ~loc (Seq (asgn, last_expr)))
      in
      mk ~loc (Let { id = arr; bound = mk ~loc (Alloc); body = asgns } )
  | Function f | ArrowFunction f -> convert_func f
  | AsConstExpression { expression; _ } -> convert_expr expression
  | AsExpression { expression; _ } -> convert_expr expression
  | Assignment _ -> raise NotImplemented
  | Binary { operator; left; right; _ } ->
      let left = convert_expr left in
      let right = convert_expr right in
      mk ~loc (Bop { op = convert_bop operator; left; right })
  | Call { callee; arguments; _ } ->
      let callee = convert_expr callee in
      convert_call callee arguments
  | Class _ -> raise NotImplemented
  | Conditional { test; consequent; alternate; _ } ->
      let test = convert_expr test in
      let consequent = convert_expr consequent in
      let alternate = convert_expr alternate in
      mk ~loc (Cond { pred = test; con = consequent; alt = alternate })
  | Identifier (_, { name; _ }) -> mk ~loc (Var name)
  | Import _ -> raise NotImplemented
  | JSXElement { opening_element = _, { name; _ }; _ } ->
      (* TODO: handle opening and attributes and children *)
      let name =
        match name with
        | Identifier (_, { name; _ }) -> mk ~loc (Var name)
        | MemberExpression (_, name) ->
          let open Flow_ast.JSX.MemberExpression in
          let rec loop { _object; property = _, { name; _ }; _ } =
            let obj = match _object with
              | Identifier (_, { name; _ }) -> mk ~loc (Var name)
              | MemberExpression (_, obj) -> loop obj
            in
            mk ~loc (Get { obj; idx = mk ~loc (Const (String name) ) })
          in
          loop name
        | _ -> raise NotImplemented (* non-identifier JSX element name *)
      in
      mk ~loc (View [ mk ~loc (App { fn = name; arg = mk ~loc (Const Unit) }) ])
  | JSXFragment _ ->
      (* TODO *)
      mk ~loc (View [ mk ~loc (Const Unit) ])
  | StringLiteral { value; _ } -> mk ~loc (Const (String value))
  | BooleanLiteral { value; _ } -> mk ~loc (Const (Bool value))
  | NullLiteral _ ->
      (* TODO: discriminate null and undefined *)
      mk ~loc (Const Unit)
  | NumberLiteral { value; _ } ->
      (* TODO: handle non-int value *)
      mk ~loc (Const (Int (Int.of_float value)))
  | BigIntLiteral { value = Some value; _ } -> mk ~loc (Const (Int (value |> Int64.to_int_exn)))
  | BigIntLiteral { value = None; _ } -> raise NotImplemented
  | RegExpLiteral _ -> raise NotImplemented
  | ModuleRefLiteral _ -> raise NotImplemented
  | Logical { operator; left; right; _ } -> (
      let left = convert_expr left in
      let right = convert_expr right in
      match operator with
      | Or ->
          (* a || b --> let a' = a in (if a' then a' else b) *)
          let name = fresh () in
          mk ~loc (Let
            {
              id = name;
              bound = left;
              body = mk ~loc (Cond { pred = mk ~loc (Var name); con = mk ~loc (Var name); alt = right });
            })
      | And ->
          (* a && b --> let a' = a in (if a' then b else a') *)
          let name = fresh () in
          mk ~loc (Let
            {
              id = name;
              bound = left;
              body = mk ~loc (Cond { pred = mk ~loc (Var name); con = right; alt = mk ~loc (Var name) });
            })
      | NullishCoalesce ->
          (* a ?? b --> let a' = a in (if a' = () then b else a') *)
          let name = fresh () in
          mk ~loc (Let
            {
              id = name;
              bound = left;
              body =
                mk ~loc (Cond
                  {
                    pred = mk ~loc (Bop { op = Eq; left = mk ~loc (Var name); right = mk ~loc (Const Unit) });
                    con = right;
                    alt = mk ~loc (Var name);
                  });
            }))
  | Member { _object; property; _ } ->
      let obj = convert_expr _object in
      convert_member obj property
  | MetaProperty _ -> raise NotImplemented
  | New _ -> raise NotImplemented
  | Object { properties; _ } ->
      (* TODO: keys should be converted to string *)
      (* { a: x, b: y } --> (let obj = {} in obj.a := x; obj.b := y; obj) *)
      let obj = fresh () in
      let convert_key_to_set prop_value = function
        | Flow_ast.Expression.Object.Property.StringLiteral (_, { value; _ }) ->
            mk ~loc (Set
              { obj = mk ~loc (Var obj); idx = mk ~loc (Const (String value)); value = prop_value })
        | NumberLiteral (_, { value; _ }) ->
            mk ~loc (Set
              {
                obj = mk ~loc (Var obj);
                idx = mk ~loc (Const (Int (Int.of_float value)));
                value = prop_value;
              })
        | BigIntLiteral (_, { value = Some value; _ }) ->
            mk ~loc (Set
              {
                obj = mk ~loc (Var obj);
                idx = mk ~loc (Const (Int (Int64.to_int_exn value)));
                value = prop_value;
              })
        | BigIntLiteral (_, { value = None; _ }) -> raise NotImplemented
        | Identifier (_, { name; _ }) ->
            mk ~loc (Set { obj = mk ~loc (Var obj); idx = mk ~loc (Const (String name)); value = prop_value })
        | PrivateName _ -> raise NotImplemented
        | Computed (_, { expression; _ }) ->
            let idx = convert_expr expression in
            mk ~loc (Set { obj = mk ~loc (Var obj); idx; value = prop_value })
      in
      let asgns =
        List.map properties ~f:(function
          | Property (_, Init { key; value; _ }) ->
              let value = convert_expr value in
              convert_key_to_set value key
          | Property (_, Method { key; value = _, value; _ }) ->
              let value = convert_func value in
              convert_key_to_set value key
          | Property (_, Get _) -> raise NotImplemented
          | Property (_, Set _) -> raise NotImplemented
          | SpreadProperty _ -> raise NotImplemented)
      in
      let body =
        asgns |> List.rev
        |> List.fold ~init:(mk ~loc (Var obj)) ~f:(fun last_expr asgn ->
               mk ~loc (Seq (asgn, last_expr)))
      in
      mk ~loc (Let { id = obj; bound = mk ~loc (Alloc); body })
  | OptionalCall { optional; call = { callee; arguments; _ }; _ } ->
      (* f?.(x) --> let f' = f in (if f' = () then () else (f' x)) *)
      let callee = convert_expr callee in
      let name = fresh () in
      if optional then
        mk ~loc (Let
          {
            id = name;
            bound = callee;
            body =
              mk ~loc (Cond
                {
                  pred = mk ~loc (Bop { op = Eq; left = mk ~loc (Var name); right = mk ~loc (Const Unit) });
                  con = mk ~loc (Const Unit);
                  alt = convert_call (mk ~loc (Var name)) arguments;
                });
          })
      else convert_call callee arguments
  | OptionalMember { optional; member; _ } ->
      (* obj?.x --> let obj' = obj in (if obj' = () then () else obj'.x) *)
      let obj = convert_expr member._object in
      if optional then
        let name = fresh () in
        mk ~loc (Let
          {
            id = name;
            bound = obj;
            body =
              mk ~loc (Cond
                {
                  pred = mk ~loc (Bop { op = Eq; left = mk ~loc (Var name); right = mk ~loc (Const Unit) });
                  con = mk ~loc (Const Unit);
                  alt = convert_member (mk ~loc (Var name)) member.property;
                });
          })
      else convert_member obj member.property
  | Sequence { expressions; _ } ->
      List.fold expressions ~init:(mk ~loc (Const Unit)) ~f:(fun left right ->
          mk ~loc (Seq (left, convert_expr right)))
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
      | Minus -> mk ~loc (Uop { op = Uminus; arg = argument })
      | Plus -> mk ~loc (Uop { op = Uplus; arg = argument })
      | Not -> mk ~loc (Uop { op = Not; arg = argument })
      | BitNot -> raise NotImplemented
      | Typeof -> raise NotImplemented
      | Void -> mk ~loc (Seq (argument, mk ~loc (Const Unit)))
      | Delete -> raise NotImplemented
      | Await -> raise NotImplemented)
  | Update _ -> raise NotImplemented
  | Yield _ -> raise NotImplemented
  | Match _ -> raise NotImplemented

and convert_pattern ((_, pattern) : (Loc.t, Loc.t) Flow_ast.Pattern.t)
    ~(base_expr : Syntax.Expr.hook_free_t) :
    (string * Syntax.Expr.hook_free_t) list =
  let open Syntax.Expr in
  match pattern with
  | Identifier { name = _, { name; _ }; _ } -> [ (name, base_expr) ]
  | Object { properties; _ } ->
      let base_name = fresh () in
      (base_name, base_expr)
      :: List.concat_map properties ~f:(function
           | Property (_, { key; pattern; default = None; _ }) ->
               let key =
                 match key with
                 | StringLiteral (_, { value; _ }) -> mk ~loc (Const (String value))
                 | NumberLiteral (_, { value; _ }) ->
                     mk ~loc (Const (Int (Int.of_float value)))
                 | BigIntLiteral (_, { value = Some value; _ }) ->
                     mk ~loc (Const (Int (Int64.to_int_exn value)))
                 | BigIntLiteral (_, { value = None; _ }) -> raise NotImplemented
                 | Identifier (_, { name; _ }) -> mk ~loc (Const (String name))
                 | Computed (_, { expression; _ }) -> convert_expr expression
               in
               convert_pattern pattern
                 ~base_expr:(mk ~loc (Get { obj = mk ~loc (Var base_name); idx = key }))
           | Property (_, { default = Some _; _ }) -> raise NotImplemented
           | RestElement _ -> raise NotImplemented)
  | Array { elements; _ } ->
      let base_name = fresh () in
      (base_name, base_expr)
      :: List.concat_mapi elements ~f:(fun i -> function
           | Element (_, { argument; default = None }) ->
               convert_pattern argument
                 ~base_expr:(mk ~loc (Get { obj = mk ~loc (Var base_name); idx = mk ~loc (Const (Int i)) }))
           | Element (_, { default = Some _; _ }) -> raise NotImplemented
           | RestElement _ -> raise NotImplemented
           | Hole _ -> [])
  | Expression _ -> raise Unreachable

let convert (js_ast : js_ast) : Syntax.Prog.t =
  let _, { Flow_ast.Program.statements; _ } = js_ast in
  let comps, stats =
    List.partition_map statements ~f:(fun stmt ->
        let _, stmt' = stmt in
        match stmt' with
        (* (* component declaration *) | ComponentDeclaration { id = _, { name;
           _ }; params = _, params; body = _, { body; _ }; _; } -> First { name;
           param = "@@param"; Syntax.Prog.body = body |> convert_stat_list |>
           Syntax.Expr.hook_full; } (* function declaration, name starting with
           uppercase, with single parameter *) | FunctionDeclaration { id = Some
           (_, { name; _ }); params = ( _, { params = [ (_, { argument; default
           = None }) ]; this_ = None; rest = None; _; } ); body; _; } when
           String.get name 0 |> Char.is_uppercase -> let param_name = fresh ()
           in let param_bindings = convert_pattern argument
           ~base_name:param_name in (* js function body *) let body = match body
           with | BodyBlock (_, { body; _ }) -> convert_stat_list body |
           BodyExpression expr -> convert_expr expr in (* function body with
           parameter destructuring *) let body = List.rev param_bindings |>
           List.fold ~init:body ~f:(fun last_expr (name, expr) -> Ex (Let { id =
           name; bound = expr; body = last_expr |> Syntax.Expr.hook_full; })) in
           First { name; param = param_name; Syntax.Prog.body = body |>
           Syntax.Expr.hook_full; } (* single variable declaration with function
           expression *) | VariableDeclaration { declarations = [ ( _, { id = _,
           Identifier { name = _, { name; _ }; _ }; init = Some (_, Function {
           body; _ }); } ); ]; _; } -> let body = match body with | BodyBlock
           (_, { body; _ }) -> convert_stat_list body | BodyExpression expr ->
           convert_expr expr in First { name; body = body |>
           Syntax.Expr.hook_full } *)
        | _ -> Second stmt)
  in
  let last_expr, _ = convert_stat_list stats in
  List.rev comps
  |> List.fold ~init:(Syntax.Prog.Expr last_expr) ~f:(fun last_expr comp ->
         Syntax.Prog.Comp (comp, last_expr))
