open! Core

type js_ast = (Loc.t, Loc.t) Flow_ast.Program.t

let parse (filename : string) : js_ast * (Loc.t * Parse_error.t) list =
  let contents = In_channel.read_all filename in

  Parser_flow.program_file ~fail:false
    ~parse_options:
      (Some { Parser_env.default_parse_options with components = true })
    contents (Some (File_key.SourceFile filename))

let show (js_ast : js_ast) : string = Flow_ast.Program.show Loc.pp Loc.pp js_ast

let some_seq e1 e2 =
  let open Syntax.Expr in
  match (hook_free e1, hook_free e2) with
  | Some e1, Some e2 -> Ex (Seq (e1, e2))
  | _, _ -> Ex (Seq (hook_full e1, hook_full e2))

let fresh =
  let counter = ref 0 in
  fun () ->
    let n = !counter in
    counter := n + 1;
    "@@" ^ Int.to_string n

let get_exn ?(msg = "option is None") : 'a option -> 'a = function
  | Some x -> x
  | None -> failwith msg
(* convert_pattern: converts js pattern declaration to a list of (name, expr) pairs.
   e. g.
   JS: let {x, y} = f(); ...
   1. call convert_pattern {x, y} ~base_name:"@@temp1"
      -> [("x", @@temp1.x); ("y", @@temp1.y)]
   2. build expression
      -> let @@temp1 = f() in
         let x = @@temp1.x in
         let y = @@temp1.y in ...
*)
[@@ocamlformat "wrap-comments=false"]

let rec convert_pattern ((_, pattern) : (Loc.t, Loc.t) Flow_ast.Pattern.t)
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
                 | StringLiteral (_, { value; _ }) -> Const (String value)
                 | NumberLiteral (_, { value; _ }) -> Const (Int (Int.of_float value))
                 | BigIntLiteral _ -> failwith "TODO"
                 | Identifier (_, { name; _ }) -> Const (String name)
                 | Computed _ -> failwith "TODO"
               in
               convert_pattern pattern
                 ~base_expr:
                   (Get { obj = Var base_name; idx = key })
           | Property (_, { default = Some _; _ }) -> failwith "TODO"
           | RestElement _ -> failwith "TODO")
  | Array { elements; _ } ->
      let base_name = fresh () in
      (base_name, base_expr)
      :: List.concat_mapi elements ~f:(fun i -> function
           | Element (_, { argument; default = None }) ->
               convert_pattern argument
                 ~base_expr:
                   (Get
                      {
                        obj = Var base_name;
                        idx = Const (Int i);
                      })
           | Element (_, { default = Some _; _ }) -> failwith "TODO"
           | RestElement _ -> failwith "TODO"
           | Hole _ -> [])
  | Expression _ -> failwith "Unreachable"

let convert_bop (bop : Flow_ast.Expression.Binary.operator) : Syntax.Expr.bop =
  let open Syntax.Expr in
  match bop with
  | Equal -> failwith "TODO"
  | NotEqual -> failwith "TODO"
  | StrictEqual -> Eq
  | StrictNotEqual -> Ne
  | LessThan -> Lt
  | LessThanEqual -> Le
  | GreaterThan -> Gt
  | GreaterThanEqual -> Ge
  | LShift -> failwith "TODO"
  | RShift -> failwith "TODO"
  | RShift3 -> failwith "TODO"
  | Plus -> Plus
  | Minus -> Minus
  | Mult -> Times
  | Exp -> failwith "TODO"
  | Div -> failwith "TODO"
  | Mod -> failwith "TODO"
  | BitOr -> failwith "TODO"
  | Xor -> failwith "TODO"
  | BitAnd -> failwith "TODO"
  | In -> failwith "TODO"
  | Instanceof -> failwith "TODO"

let rec convert_stat_list (body : (Loc.t, Loc.t) Flow_ast.Statement.t list) :
    Syntax.Expr.hook_free_t =
  let open Syntax.Expr in
  let rec convert_stat tail ((_, stmt) : (Loc.t, Loc.t) Flow_ast.Statement.t) =
    match stmt with
    | Block { body; _ } ->
        let body = convert_stat_list body in
        Seq (body, tail)
    | Break _ -> failwith "TODO"
    | ClassDeclaration _ -> failwith "TODO"
    | ComponentDeclaration _ -> failwith "TODO"
    | Continue _ -> failwith "TODO"
    | Debugger _ -> tail
    | DeclareClass _ | DeclareComponent _ | DeclareEnum _
    | DeclareExportDeclaration _ | DeclareFunction _ | DeclareInterface _
    | DeclareModule _ | DeclareModuleExports _ | DeclareNamespace _
    | DeclareTypeAlias _ | DeclareOpaqueType _ | DeclareVariable _ ->
        (* flow statements starting with 'declare' *)
        tail
    | DoWhile _ -> failwith "TODO"
    | Empty _ -> tail
    | EnumDeclaration _ -> failwith "TODO"
    | ExportDefaultDeclaration { declaration; _ } -> (
        (* TODO: handle export default declaration *)
        match declaration with
        | Declaration stmt ->
            (* delegate to var and function declaration *)
            convert_stat tail stmt
        | Expression expr -> Seq (convert_expr expr, tail))
    | ExportNamedDeclaration { declaration; _ } -> (
        (* TODO: handle export named declaration, especially those without
           declaration *)
        match declaration with
        | Some stmt ->
            (* delegate to var and function declaration *)
            convert_stat tail stmt
        | None -> tail)
    | Expression { expression; _ } ->
        let expr = convert_expr expression in
        Seq (expr, tail)
    | For _ -> failwith "TODO"
    | ForIn _ -> failwith "TODO"
    | ForOf _ -> failwith "TODO"
    | FunctionDeclaration f -> (
        let expr = convert_func f in
        match f.id with
        | Some (_, { name; _ }) -> Let { id = name; bound = expr; body = tail }
        | None -> Seq (expr, tail))
    | If { test; consequent; alternate; _ } ->
        let test = convert_expr test in
        let consequent = convert_stat (Const Unit) consequent in
        let alternate =
          match alternate with
          | Some (_, { body; _ }) -> convert_stat (Const Unit) body
          | None -> Const Unit
        in
        Seq (Cond { pred = test; con = consequent; alt = alternate }, tail)
    | ImportDeclaration _ -> failwith "TODO"
    | InterfaceDeclaration _ -> failwith "TODO"
    | Labeled _ ->
        (* TODO: handle labeled statement *)
        tail
    | Return _ -> failwith "TODO"
    | Switch _ -> failwith "TODO"
    | Throw _ -> failwith "TODO"
    | Try _ -> failwith "TODO"
    | TypeAlias _ | OpaqueType _ ->
        (* flow type declaration *)
        tail
    | VariableDeclaration { declarations; _ } ->
        let decls =
          List.concat_map declarations ~f:(fun (_, { id; init; _ }) ->
              let init =
                match init with
                | Some expr -> convert_expr expr
                | None -> Const Unit
              in
              convert_pattern id ~base_expr:init)
        in
        decls |> List.rev
        |> List.fold ~init:tail ~f:(fun tail (name, expr) ->
               Let { id = name; bound = expr; body = tail })
    | While _ -> failwith "TODO"
    | With _ -> failwith "TODO"
  in
  List.rev body |> List.fold ~init:(Const Unit) ~f:convert_stat

and convert_func ({ params; body; _ } : (Loc.t, Loc.t) Flow_ast.Function.t) :
    Syntax.Expr.hook_free_t =
  (* TODO: handle recursive binding *)
  let open Syntax.Expr in
  let param =
    match params with
    | _, { params = [ (_, { argument; default = None }) ]; _ } -> argument
    | _ -> failwith "TODO: non-single or optional parameter"
  in
  let param_name, param_bindings =
    match param with
    | _, Identifier { name = _, { name; _ }; _ } -> (name, [])
    | _ ->
        let param_name = fresh () in
        let param_bindings =
          convert_pattern param ~base_expr:(Var param_name)
        in
        (param_name, param_bindings)
  in
  let body =
    match body with
    | BodyBlock (_, { body; _ }) -> convert_stat_list body
    | BodyExpression expr -> convert_expr expr
  in
  let body =
    List.rev param_bindings
    |> List.fold ~init:body ~f:(fun last_expr (name, expr) ->
           Let { id = name; bound = expr; body = last_expr })
  in
  Fn { param = param_name; body }

and convert_call (callee : Syntax.Expr.hook_free_t)
    ((_, { arguments; _ }) : (Loc.t, Loc.t) Flow_ast.Expression.ArgList.t) :
    Syntax.Expr.hook_free_t =
  let argument =
    match arguments with
    | [ Expression expr ] -> convert_expr expr
    | _ -> failwith "TODO: non-single or spread arguments"
  in
  App { fn = callee; arg = argument }

and convert_member (obj : Syntax.Expr.hook_free_t)
    (property : (Loc.t, Loc.t) Flow_ast.Expression.Member.property) :
    Syntax.Expr.hook_free_t =
  let open Syntax.Expr in
  match property with
  | PropertyIdentifier (_, { name; _ }) ->
      Get { obj; idx = Const (String name) }
  | PropertyPrivateName _ -> failwith "TODO"
  | PropertyExpression expr -> Get { obj; idx = convert_expr expr }

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
                Set { obj = Var arr; idx = Const (Int i); value = elem }
            | _ -> failwith "TODO")
      in
      let asgns =
        asgns |> List.rev
        |> List.fold ~init:(Var arr) ~f:(fun last_expr asgn ->
               Seq (asgn, last_expr))
      in
      Let { id = arr; bound = Alloc; body = asgns }
  | Function f | ArrowFunction f -> convert_func f
  | AsConstExpression { expression; _ } -> convert_expr expression
  | AsExpression { expression; _ } -> convert_expr expression
  | Assignment _ -> failwith "TODO"
  | Binary { operator; left; right; _ } ->
      let left = convert_expr left in
      let right = convert_expr right in
      Bop { op = convert_bop operator; left; right }
  | Call { callee; arguments; _ } ->
      let callee = convert_expr callee in
      convert_call callee arguments
  | Class _ -> failwith "TODO"
  | Conditional { test; consequent; alternate; _ } ->
      let test = convert_expr test in
      let consequent = convert_expr consequent in
      let alternate = convert_expr alternate in
      Cond { pred = test; con = consequent; alt = alternate }
  | Identifier (_, { name; _ }) -> Var name
  | Import _ -> failwith "TODO"
  | JSXElement { opening_element = _, { name; _ }; _ } ->
      (* TODO: handle opening and attributes and children *)
      let name =
        match name with
        | Identifier (_, { name; _ }) -> name
        | _ -> failwith "TODO: non-identifier JSX element name"
      in
      View [ App { fn = Var name; arg = Const Unit } ]
  | JSXFragment _ ->
      (* TODO *)
      View [ Const Unit ]
  | StringLiteral _ -> failwith "TODO"
  | BooleanLiteral { value; _ } -> Const (Bool value)
  | NullLiteral _ ->
      (* TODO: discriminate null and undefined *)
      Const Unit
  | NumberLiteral { value; _ } ->
      (* TODO: handle non-int value *)
      Const (Int (Int.of_float value))
  | BigIntLiteral _ -> failwith "TODO"
  | RegExpLiteral _ -> failwith "TODO"
  | ModuleRefLiteral _ -> failwith "Not Supported"
  | Logical { operator; left; right; _ } -> (
      let left = convert_expr left in
      let right = convert_expr right in
      match operator with
      | Or -> Cond { pred = left; con = Const (Bool true); alt = right }
      | And -> Cond { pred = left; con = right; alt = Const (Bool false) }
      | NullishCoalesce -> failwith "TODO")
  | Member { _object; property; _ } ->
      let obj = convert_expr _object in
      convert_member obj property
  | MetaProperty _ -> failwith "TODO"
  | New _ -> failwith "TODO"
  | Object { properties; _ } ->
      (* { a: x, b: y } --> (let obj = {} in obj.a := x; obj.b := y; obj) *)
      let obj = fresh () in
      let convert_key_to_set prop_value = function
        | Flow_ast.Expression.Object.Property.StringLiteral (_, { value; _ }) ->
            Set
              { obj = Var obj; idx = Const (String value); value = prop_value }
        | NumberLiteral (_, { value; _ }) ->
            Set
              {
                obj = Var obj;
                idx = Const (Int (Int.of_float value));
                value = prop_value;
              }
        | BigIntLiteral _ -> failwith "TODO"
        | Identifier (_, { name; _ }) ->
            Set { obj = Var obj; idx = Const (String name); value = prop_value }
        | PrivateName _ -> failwith "TODO"
        | Computed _ -> failwith "TODO"
      in
      let asgns =
        List.map properties ~f:(function
          | Property (_, Init { key; value; _ }) ->
              let value = convert_expr value in
              convert_key_to_set value key
          | Property (_, Method { key; value = _, value; _ }) ->
              let value = convert_func value in
              convert_key_to_set value key
          | Property (_, Get _) -> failwith "TODO"
          | Property (_, Set _) -> failwith "TODO"
          | SpreadProperty _ -> failwith "TODO")
      in
      let body =
        asgns |> List.rev
        |> List.fold ~init:(Var obj) ~f:(fun last_expr asgn ->
               Seq (asgn, last_expr))
      in
      Let { id = obj; bound = Alloc; body }
  | OptionalCall { optional; call = { callee; arguments; _ }; _ } ->
      (* f?.(x) --> let f' = f in (if f' = () then () else (f' x)) *)
      let callee = convert_expr callee in
      let name = fresh () in
      if optional then
        Let
          {
            id = name;
            bound = callee;
            body =
              Cond
                {
                  pred = Bop { op = Eq; left = Var name; right = Const Unit };
                  con = Const Unit;
                  alt = convert_call (Var name) arguments;
                };
          }
      else convert_call callee arguments
  | OptionalMember { optional; member; _ } ->
      (* obj?.x --> let obj' = obj in (if obj' = () then () else obj'.x) *)
      let obj = convert_expr member._object in
      if optional then
        let name = fresh () in
        Let
          {
            id = name;
            bound = obj;
            body =
              Cond
                {
                  pred = Bop { op = Eq; left = Var name; right = Const Unit };
                  con = Const Unit;
                  alt = convert_member (Var name) member.property;
                };
          }
      else convert_member obj member.property
  | Sequence { expressions; _ } ->
      List.fold expressions ~init:(Const Unit) ~f:(fun left right ->
          Seq (left, convert_expr right))
  | Super _ -> failwith "TODO"
  | TaggedTemplate _ -> failwith "TODO"
  | TemplateLiteral _ -> failwith "TODO"
  | This _ -> failwith "TODO"
  | TypeCast { expression; _ } -> convert_expr expression
  | TSSatisfies { expression; _ } -> convert_expr expression
  | Unary { operator; argument; _ } -> (
      let argument = convert_expr argument in
      let open Syntax.Expr in
      match operator with
      | Minus -> Uop { op = Uminus; arg = argument }
      | Plus -> Uop { op = Uplus; arg = argument }
      | Not -> Uop { op = Not; arg = argument }
      | BitNot -> failwith "TODO"
      | Typeof -> failwith "TODO"
      | Void -> Seq (argument, Const Unit)
      | Delete -> failwith "TODO"
      | Await -> failwith "TODO")
  | Update _ -> failwith "TODO"
  | Yield _ -> failwith "TODO"

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
  let last_expr = stats |> convert_stat_list in
  List.rev comps
  |> List.fold ~init:(Syntax.Prog.Expr last_expr) ~f:(fun last_expr comp ->
         Syntax.Prog.Comp (comp, last_expr))
