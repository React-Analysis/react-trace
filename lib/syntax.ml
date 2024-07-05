open! Base

module Id = struct
  include Util.Map_key (String)

  let unit = "()"
end

module Label = struct
  module T = Int
  include T

  module Map = struct
    open Map
    include M (T)

    let empty = empty (module T)
  end
end

type hook_free = private Hook_free
type hook_full = private Hook_full

module Expr = struct
  type _ t =
    | Const : const -> _ t
    | Var : Id.t -> _ t
    | View : hook_free t list -> _ t
    | Cond : { pred : hook_free t; con : hook_free t; alt : hook_free t } -> _ t
    | Fn : { param : Id.t; body : hook_free t } -> _ t
    | App : { fn : hook_free t; arg : hook_free t } -> _ t
    | Let : { id : Id.t; bound : hook_free t; body : 'a t } -> 'a t
    | Stt : {
        label : Label.t;
        stt : Id.t;
        set : Id.t;
        init : hook_free t;
        body : hook_full t;
      }
        -> hook_full t
    | Eff : hook_free t -> hook_full t
    | Seq : 'a t * 'a t -> 'a t
    | Bin_op : { op : bin_op; left : hook_free t; right : hook_free t } -> _ t

  and const = Unit | Bool of bool | Int of int
  and bin_op = And | Or | Plus | Minus | Times

  type some_expr = Ex : 'a t -> some_expr [@@unboxed]

  let rec hook_free (expr : some_expr) : hook_free t option =
    let (Ex expr) = expr in
    let ( let* ) = Stdlib.Option.bind in
    match expr with
    | Const _ as e -> Some e
    | Var _ as e -> Some e
    | View _ as e -> Some e
    | Cond _ as e -> Some e
    | Fn _ as e -> Some e
    | App _ as e -> Some e
    | Let ({ body; _ } as e) ->
        let* body = hook_free (Ex body) in
        Some (Let { e with body })
    | Stt _ -> None
    | Eff _ -> None
    | Seq (e1, e2) ->
        let* e1 = hook_free (Ex e1) in
        let* e2 = hook_free (Ex e2) in
        Some (Seq (e1, e2))
    | Bin_op _ as e -> Some e

  let hook_free_exn e = Option.value_exn (hook_free e)

  let rec hook_full (expr : some_expr) : hook_full t =
    let (Ex expr) = expr in
    match expr with
    | Const _ as e -> e
    | Var _ as e -> e
    | View _ as e -> e
    | Cond _ as e -> e
    | Fn _ as e -> e
    | App _ as e -> e
    | Let ({ body; _ } as e) ->
        let body = hook_full (Ex body) in
        Let { e with body }
    | Stt _ as e -> e
    | Eff _ as e -> e
    | Seq (e1, e2) ->
        let e1 = hook_full (Ex e1) in
        let e2 = hook_full (Ex e2) in
        Seq (e1, e2)
    | Bin_op _ as e -> e

  let string_of_bin_op = function
    | And -> "&&"
    | Or -> "||"
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"

  let rec sexp_of_t : type a. a t -> Sexp.t =
    let open Sexp_helper in
    function
    | Const Unit -> a "()"
    | Const (Bool b) -> Bool.sexp_of_t b
    | Const (Int i) -> Int.sexp_of_t i
    | Var id -> Id.sexp_of_t id
    | View es -> l (a "View" :: List.map ~f:sexp_of_t es)
    | Cond { pred; con; alt } ->
        l [ a "Cond"; sexp_of_t pred; sexp_of_t con; sexp_of_t alt ]
    | Fn { param; body } -> l [ a "Fn"; Id.sexp_of_t param; sexp_of_t body ]
    | App { fn; arg } -> l [ a "App"; sexp_of_t fn; sexp_of_t arg ]
    | Let { id; bound; body } ->
        l [ a "Let"; Id.sexp_of_t id; sexp_of_t bound; sexp_of_t body ]
    | Stt { label; stt; set; init; body } ->
        l
          [
            a "Stt";
            Label.sexp_of_t label;
            Id.sexp_of_t stt;
            Id.sexp_of_t set;
            sexp_of_t init;
            sexp_of_t body;
          ]
    | Eff e -> l [ a "Eff"; sexp_of_t e ]
    | Seq (e1, e2) -> l [ a "Seq"; sexp_of_t e1; sexp_of_t e2 ]
    | Bin_op { op; left; right } ->
        l
          [
            a "Bin_op"; a (string_of_bin_op op); sexp_of_t left; sexp_of_t right;
          ]
end

module Prog = struct
  type t = Expr of hook_free Expr.t | Comp of (comp * t)
  and comp = { name : Id.t; param : Id.t; body : hook_full Expr.t }

  let sexp_of_comp ({ name; param; body } : comp) : Sexp.t =
    let open Sexp_helper in
    l [ a "Comp"; Id.sexp_of_t name; Id.sexp_of_t param; Expr.sexp_of_t body ]

  let rec sexp_of_t : t -> Sexp.t =
    let open Sexp_helper in
    function
    | Expr e -> l [ a "Expr"; Expr.sexp_of_t e ]
    | Comp (comp, prog) -> l [ sexp_of_comp comp; sexp_of_t prog ]
end
