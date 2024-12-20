open! Base

module Map_key (T : sig
  type t

  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
  val of_string : string -> t
  val to_string : t -> string
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val compare : t -> t -> int
  val min : t -> t -> t
  val max : t -> t -> t
  val ascending : t -> t -> int
  val descending : t -> t -> int
  val between : t -> low:t -> high:t -> bool
  val clamp_exn : t -> min:t -> max:t -> t
  val clamp : t -> min:t -> max:t -> t Or_error.t

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
  val hash : t -> int
  val equal : t -> t -> bool
end) =
struct
  include T

  module Map = struct
    open Map
    include M (T)

    let empty = empty (module T)
    let sexp_of_t sexp_of_v t = Map.sexp_of_m__t (module T) sexp_of_v t
  end
end

module Id = struct
  include Map_key (String)

  let unit = "()"
end

module Label = struct
  include Map_key (Int)
end

module Expr = struct
  type hook_free = private Hook_free
  type hook_full = private Hook_full
  type const = Unit | Bool of bool | Int of int | String of string
  type uop = Not | Uplus | Uminus
  type bop = Eq | Lt | Gt | Ne | Le | Ge | And | Or | Plus | Minus | Times

  type 'a t = { desc : 'a desc; loc : Location.t }

  and _ desc =
    | Const : const -> _ desc
    | Var : Id.t -> _ desc
    | View : hook_free t list -> _ desc
    | Cond : {
        pred : hook_free t;
        con : hook_free t;
        alt : hook_free t;
      }
        -> _ desc
    | Fn : { param : Id.t; body : hook_free t } -> _ desc
    | App : { fn : hook_free t; arg : hook_free t } -> _ desc
    | Let : { id : Id.t; bound : hook_free t; body : 'a t } -> 'a desc
    | Stt : {
        label : Label.t;
        stt : Id.t;
        set : Id.t;
        init : hook_free t;
        body : hook_full t;
      }
        -> hook_full desc
    | Eff : hook_free t -> hook_full desc
    | Seq : 'a t * 'a t -> 'a desc
    | Uop : { op : uop; arg : hook_free t } -> _ desc
    | Bop : { op : bop; left : hook_free t; right : hook_free t } -> _ desc
    | Alloc : _ desc
    | Get : { obj : hook_free t; idx : hook_free t } -> _ desc
    | Set : {
        obj : hook_free t;
        idx : hook_free t;
        value : hook_free t;
      }
        -> _ desc

  type hook_free_t = hook_free t
  type hook_full_t = hook_full t
  type some_expr = Ex : 'a t -> some_expr [@@unboxed]

  let mk ~loc desc = { desc; loc }

  let rec hook_free (expr : some_expr) : hook_free t option =
    let (Ex { desc; loc }) = expr in
    let mk = mk ~loc in
    let ( let* ) x f = Option.bind x ~f in
    match desc with
    | Let ({ body; _ } as e) ->
        let* body = hook_free (Ex body) in
        Some (mk (Let { e with body }))
    | Stt _ | Eff _ -> None
    | Seq (e1, e2) ->
        let* e1 = hook_free (Ex e1) in
        let* e2 = hook_free (Ex e2) in
        Some (mk (Seq (e1, e2)))
    | (Const _ as e)
    | (Var _ as e)
    | (View _ as e)
    | (Cond _ as e)
    | (Fn _ as e)
    | (App _ as e)
    | (Bop _ as e)
    | (Uop _ as e)
    | (Alloc as e)
    | (Get _ as e)
    | (Set _ as e) ->
        Some (mk e)

  let hook_free_exn e = Option.value_exn (hook_free e)

  let rec hook_full (expr : some_expr) : hook_full t =
    let (Ex { desc; loc }) = expr in
    let mk = mk ~loc in
    match desc with
    | Let ({ body; _ } as e) ->
        let body = hook_full (Ex body) in
        mk (Let { e with body })
    | Seq (e1, e2) ->
        let e1 = hook_full (Ex e1) in
        let e2 = hook_full (Ex e2) in
        mk (Seq (e1, e2))
    | (Const _ as e)
    | (Var _ as e)
    | (View _ as e)
    | (Cond _ as e)
    | (Fn _ as e)
    | (App _ as e)
    | (Stt _ as e)
    | (Eff _ as e)
    | (Uop _ as e)
    | (Bop _ as e)
    | (Alloc as e)
    | (Get _ as e)
    | (Set _ as e) ->
        mk e

  let string_of_uop = function Not -> "not" | Uplus -> "+" | Uminus -> "-"

  let string_of_bop = function
    | Eq -> "="
    | Lt -> "<"
    | Gt -> ">"
    | Ne -> "<>"
    | Le -> "<="
    | Ge -> ">="
    | And -> "&&"
    | Or -> "||"
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"

  let rec sexp_of_t : type a. a t -> Sexp.t =
   fun { desc; _ } ->
    let open Sexp_helper in
    match desc with
    | Const Unit -> a "()"
    | Const (Bool b) -> Bool.sexp_of_t b
    | Const (Int i) -> Int.sexp_of_t i
    | Const (String s) -> String.sexp_of_t s
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
    | Uop { op; arg } -> l [ a "Uop"; a (string_of_uop op); sexp_of_t arg ]
    | Bop { op; left; right } ->
        l [ a "Bop"; a (string_of_bop op); sexp_of_t left; sexp_of_t right ]
    | Alloc -> a "Alloc"
    | Get { obj; idx } -> l [ a "Get"; sexp_of_t obj; sexp_of_t idx ]
    | Set { obj; idx; value } ->
        l [ a "Set"; sexp_of_t obj; sexp_of_t idx; sexp_of_t value ]

  let sexp_of_hook_free_t = sexp_of_t
  let sexp_of_hook_full_t = sexp_of_t
end

module Prog = struct
  type t = Expr of Expr.hook_free_t | Comp of (comp * t)
  and comp = { name : Id.t; param : Id.t; body : Expr.hook_full_t }

  let sexp_of_comp ({ name; param; body } : comp) : Sexp.t =
    let open Sexp_helper in
    l [ a "Comp"; Id.sexp_of_t name; Id.sexp_of_t param; Expr.sexp_of_t body ]

  let rec sexp_of_t : t -> Sexp.t =
    let open Sexp_helper in
    function
    | Expr e -> l [ a "Expr"; Expr.sexp_of_t e ]
    | Comp (comp, prog) -> l [ sexp_of_comp comp; sexp_of_t prog ]
end
