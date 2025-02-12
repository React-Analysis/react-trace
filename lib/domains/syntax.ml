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
    | Cond : hook_free t desc_cond -> _ desc
    | Fn : hook_free t desc_fn -> _ desc
    | App : hook_free t desc_app -> _ desc
    | Let : (hook_free t, 'a t) desc_let -> 'a desc
    | Stt : (hook_free t, hook_full t) desc_stt -> hook_full desc
    | Eff : hook_free t -> hook_full desc
    | Seq : 'a t * 'a t -> 'a desc
    | Uop : hook_free t desc_uop -> _ desc
    | Bop : hook_free t desc_bop -> _ desc
    | Alloc : _ desc
    | Get : hook_free t desc_get -> _ desc
    | Set : hook_free t desc_set -> _ desc

  and 'e desc_cond = { pred : 'e; con : 'e; alt : 'e }
  and 'body desc_fn = { self : Id.t option; param : Id.t; body : 'body }
  and 'e desc_app = { fn : 'e; arg : 'e }
  and ('bound, 'body) desc_let = { id : Id.t; bound : 'bound; body : 'body }

  and ('init, 'body) desc_stt = {
    label : Label.t;
    stt : Id.t;
    set : Id.t;
    init : 'init;
    body : 'body;
  }

  and 'e desc_uop = { op : uop; arg : 'e }
  and 'e desc_bop = { op : bop; left : 'e; right : 'e }
  and 'e desc_get = { obj : 'e; idx : 'e }
  and 'e desc_set = { obj : 'e; idx : 'e; value : 'e }

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

  let x_loc_start (e : some_expr) : Lexing.position =
    let (Ex { loc; _ }) = e in
    loc.loc_start

  let x_loc_end (e : some_expr) : Lexing.position =
    let (Ex { loc; _ }) = e in
    loc.loc_end

  let x_seq ?(loc = Location.none) ((e1, e2) : some_expr * some_expr) :
      some_expr =
    match (hook_free e1, hook_free e2) with
    | Some e1, Some e2 -> Ex (mk ~loc (Seq (e1, e2)))
    | _ -> Ex (mk ~loc (Seq (hook_full e1, hook_full e2)))

  let x_let ?(loc = Location.none) { id; bound; body } : some_expr =
    let bound = hook_free_exn bound in
    match hook_free body with
    | Some body -> Ex (mk ~loc (Let { id; bound; body }))
    | _ -> Ex (mk ~loc (Let { id; bound; body = hook_full body }))

  let x_stt ?(loc = Location.none) { label; stt; set; init; body } : some_expr =
    Ex
      (mk ~loc
         (Stt
            {
              label;
              stt;
              set;
              init = hook_free_exn init;
              body = hook_full body;
            }))

  let x_eff ?(loc = Location.none) e : some_expr =
    Ex (mk ~loc (Eff (hook_free_exn e)))

  let x_cond ?(loc = Location.none) { pred; con; alt } : some_expr =
    Ex
      (mk ~loc
         (Cond
            {
              pred = hook_free_exn pred;
              con = hook_free_exn con;
              alt = hook_free_exn alt;
            }))

  let x_alloc ?(loc = Location.none) () : some_expr = Ex (mk ~loc Alloc)
  let x_var ?(loc = Location.none) id : some_expr = Ex (mk ~loc (Var id))

  let x_set ?(loc = Location.none) { obj; idx; value } : some_expr =
    Ex
      (mk ~loc
         (Set
            {
              obj = hook_free_exn obj;
              idx = hook_free_exn idx;
              value = hook_free_exn value;
            }))

  let x_get ?(loc = Location.none) { obj; idx } : some_expr =
    Ex (mk ~loc (Get { obj = hook_free_exn obj; idx = hook_free_exn idx }))

  let x_const_string ?(loc = Location.none) s : some_expr =
    Ex (mk ~loc (Const (String s)))

  let x_const_unit ?(loc = Location.none) () : some_expr =
    Ex (mk ~loc (Const Unit))

  let x_const_bool ?(loc = Location.none) b : some_expr =
    Ex (mk ~loc (Const (Bool b)))

  let x_const_int ?(loc = Location.none) i : some_expr =
    Ex (mk ~loc (Const (Int i)))

  let x_bop ?(loc = Location.none) { op; left; right } : some_expr =
    Ex
      (mk ~loc
         (Bop { op; left = hook_free_exn left; right = hook_free_exn right }))

  let x_uop ?(loc = Location.none) { op; arg } : some_expr =
    Ex (mk ~loc (Uop { op; arg = hook_free_exn arg }))

  let x_app ?(loc = Location.none) { fn; arg } : some_expr =
    Ex (mk ~loc (App { fn = hook_free_exn fn; arg = hook_free_exn arg }))

  let x_fn ?(loc = Location.none) { self; param; body } : some_expr =
    Ex (mk ~loc (Fn { self; param; body = hook_free_exn body }))

  let x_view ?(loc = Location.none) es : some_expr =
    Ex (mk ~loc (View (List.map ~f:hook_free_exn es)))

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
   fun { desc; _ } -> sexp_of_desc desc

  and sexp_of_desc : type a. a desc -> Sexp.t =
   fun desc ->
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
    | Fn { self; param; body } ->
        l
          [
            a "Fn";
            Option.sexp_of_t Id.sexp_of_t self;
            Id.sexp_of_t param;
            sexp_of_t body;
          ]
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
