open! Core
open Syntax
module Path = Int

module rec T : sig
  type clos = { param : Id.t; body : Expr.hook_free_t; env : Env.t }
  type set_clos = { label : Label.t; path : Path.t }
  type comp_clos = { comp : Prog.comp; env : Env.t }

  type value =
    | Unit
    | Bool of bool
    | Int of int
    | View_spec of view_spec list
    | Clos of clos
    | Set_clos of set_clos
    | Comp_clos of comp_clos
    | Comp_spec of comp_spec

  and comp_spec = { comp : Prog.comp; env : Env.t; arg : value }
  and view_spec = Vs_null | Vs_int of int | Vs_comp of comp_spec

  type phase = P_init | P_update | P_retry | P_effect
  type decision = Idle | Retry | Update

  type part_view =
    | Root
    | Node of {
        comp_spec : comp_spec;
        dec : decision;
        st_store : St_store.t;
        eff_q : Job_q.t;
      }

  type tree = Leaf_null | Leaf_int of int | Path of Path.t
  type entry = { part_view : part_view; children : tree Snoc_list.t }

  val sexp_of_clos : clos -> Sexp.t
  val sexp_of_set_clos : set_clos -> Sexp.t
  val sexp_of_comp_clos : comp_clos -> Sexp.t
  val sexp_of_value : value -> Sexp.t
  val sexp_of_comp_spec : comp_spec -> Sexp.t
  val sexp_of_view_spec : view_spec -> Sexp.t
  val sexp_of_phase : phase -> Sexp.t
  val sexp_of_decision : decision -> Sexp.t
  val sexp_of_part_view : part_view -> Sexp.t
  val sexp_of_tree : tree -> Sexp.t
  val sexp_of_entry : entry -> Sexp.t
end = struct
  type clos = { param : Id.t; body : Expr.hook_free_t; env : Env.t }
  [@@deriving sexp_of]

  type value =
    | Unit
    | Bool of bool
    | Int of int
    | View_spec of view_spec list
    | Clos of clos
    | Set_clos of set_clos
    | Comp_clos of comp_clos
    | Comp_spec of comp_spec

  and set_clos = { label : Label.t; path : Path.t }
  and comp_clos = { comp : Prog.comp; env : Env.t }
  and comp_spec = { comp : Prog.comp; env : Env.t; arg : value }

  and view_spec = Vs_null | Vs_int of int | Vs_comp of comp_spec
  [@@deriving sexp_of]

  type phase = P_init | P_update | P_retry | P_effect [@@deriving sexp_of]
  type decision = Idle | Retry | Update [@@deriving sexp_of]

  type part_view =
    | Root
    | Node of {
        comp_spec : comp_spec;
        dec : decision;
        st_store : St_store.t;
        eff_q : Job_q.t;
      }
  [@@deriving sexp_of]

  type tree = Leaf_null | Leaf_int of int | Path of Path.t
  [@@deriving sexp_of]

  type entry = { part_view : part_view; children : tree Snoc_list.t }
  [@@deriving sexp_of]
end

and Env : sig
  type t

  val empty : t
  val lookup : t -> id:Id.t -> T.value option
  val extend : t -> id:Id.t -> value:T.value -> t
  val sexp_of_t : t -> Sexp.t
end = struct
  type t = T.value Id.Map.t [@@deriving sexp_of]

  let empty = Id.Map.empty
  let lookup env ~id = Map.find env id
  let extend env ~id ~value = Map.set env ~key:id ~data:value
end

and St_store : sig
  type t

  val empty : t
  val lookup : t -> label:Label.t -> T.value * Job_q.t
  val update : t -> label:Label.t -> value:T.value * Job_q.t -> t
  val sexp_of_t : t -> Sexp.t
end = struct
  type t = (T.value * Job_q.t) Label.Map.t [@@deriving sexp_of]

  let empty = Label.Map.empty
  let lookup store ~label = Map.find_exn store label
  let update store ~label ~value = Map.set store ~key:label ~data:value
end

and Job_q : (Batched_queue.S with type elt := T.clos) = Batched_queue.M (struct
  type t = T.clos

  let sexp_of_t = T.sexp_of_clos
end)

include T

module Tree_mem : sig
  type t

  val empty : t
  val lookup_st : t -> path:Path.t -> label:Label.t -> value * Job_q.t
  val update_st : t -> path:Path.t -> label:Label.t -> value * Job_q.t -> t
  val get_dec : t -> path:Path.t -> decision
  val set_dec : t -> path:Path.t -> decision -> t
  val enq_eff : t -> path:Path.t -> clos -> t
  val alloc_pt : t -> Path.t
  val lookup_ent : t -> path:Path.t -> entry
  val update_ent : t -> path:Path.t -> entry -> t
  val sexp_of_t : t -> Sexp.t
end = struct
  type t = entry Path.Map.t [@@deriving sexp_of]

  let empty = Path.Map.empty

  let lookup_st tree_mem ~path ~label =
    let { part_view; _ } = Map.find_exn tree_mem path in
    match part_view with
    | Root -> assert false
    | Node { st_store; _ } -> St_store.lookup st_store ~label

  let update_st tree_mem ~path ~label (v, q) =
    let ({ part_view; _ } as entry) = Map.find_exn tree_mem path in
    match part_view with
    | Root -> assert false
    | Node ({ st_store; _ } as n) ->
        let st_store = St_store.update st_store ~label ~value:(v, q) in
        Map.set tree_mem ~key:path
          ~data:{ entry with part_view = Node { n with st_store } }

  let get_dec tree_mem ~path =
    let { part_view; _ } = Map.find_exn tree_mem path in
    match part_view with Root -> assert false | Node { dec; _ } -> dec

  let set_dec tree_mem ~path dec =
    let ({ part_view; _ } as entry) = Map.find_exn tree_mem path in
    match part_view with
    | Root -> assert false
    | Node n ->
        Map.set tree_mem ~key:path
          ~data:{ entry with part_view = Node { n with dec } }

  let enq_eff tree_mem ~path clos =
    let ({ part_view; _ } as entry) = Map.find_exn tree_mem path in
    match part_view with
    | Root -> assert false
    | Node ({ eff_q; _ } as n) ->
        let eff_q = Job_q.enqueue eff_q clos in
        Map.set tree_mem ~key:path
          ~data:{ entry with part_view = Node { n with eff_q } }

  let alloc_pt = Map.length
  let lookup_ent tree_mem ~path = Map.find_exn tree_mem path

  let update_ent tree_mem ~path ent =
    Logs.debug (fun m -> m "update_ent: %a" Sexp.pp_hum (Path.sexp_of_t path));
    Map.set tree_mem ~key:path ~data:ent
end

module Value = struct
  type t = value

  let to_bool = function Bool b -> Some b | _ -> None
  let to_int = function Int i -> Some i | _ -> None

  let to_vs = function
    | Unit -> Some Vs_null
    | Int i -> Some (Vs_int i)
    | Comp_spec t -> Some (Vs_comp t)
    | _ -> None

  let to_vss = function View_spec vss -> Some vss | _ -> None
  let to_clos = function Clos c -> Some c | _ -> None

  let equal v1 v2 =
    match (v1, v2) with
    | Unit, Unit -> true
    | Bool b1, Bool b2 -> Bool.(b1 = b2)
    | Int i1, Int i2 -> i1 = i2
    | _, _ -> false

  let ( = ) = equal
  let ( <> ) v1 v2 = not (v1 = v2)
end

module Phase = struct
  type t = phase = P_init | P_update | P_retry | P_effect [@@deriving equal]

  let ( = ) = equal
  let ( <> ) p1 p2 = not (p1 = p2)
end

module Decision = struct
  type t = decision = Idle | Retry | Update [@@deriving equal]

  let ( = ) = equal
  let ( <> ) d1 d2 = not (d1 = d2)
end
