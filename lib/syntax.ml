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

  and const = Unit of unit | Int of int
  and bin_op = Plus | Minus | Times
end

module Prog = struct
  type t = Expr of hook_free Expr.t | Comp of (comp * t)
  and comp = { name : Id.t; param : Id.t; body : hook_full Expr.t }
end
