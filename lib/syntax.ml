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

  and const = Unit | Int of int
  and bin_op = Plus | Minus | Times

  let string_of_bin_op = function Plus -> "+" | Minus -> "-" | Times -> "*"

  let rec sexp_of_t : type a. a t -> Sexp.t =
    let open Sexp_helper in
    function
    | Const Unit -> a "()"
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
