open! Base
open Syntax
module Path = Util.Map_key (Int)

module rec T : sig
  type value =
    | Unit
    | Int of int
    | View_spec of view_spec list
    | Clos of clos
    | Set_clos of set_clos
    | Comp_clos of comp_clos
    | Comp_thunk of comp_thunk

  and clos = { param : Id.t; body : hook_free Expr.t; env : Env.t }
  and set_clos = { label : Label.t; path : Path.t }
  and comp_clos = { comp : Prog.comp; env : Env.t }
  and comp_thunk = { comp : Prog.comp; env : Env.t; arg : value }
  and view_spec = Vs_null | Vs_int of int | Vs_comp of comp_thunk

  type phase = P_init | P_update | P_retry | P_effect | P_top
  and decision = Idle | Retry | Update
  and st_store = St_store.t
  and job_q = Job_q.t

  and part_view =
    | Root
    | Node of {
        view_spec : view_spec;
        dec : decision;
        st_store : St_store.t;
        eff_q : Job_q.t;
      }

  and tree = Leaf_null | Leaf_int of int | Path of Path.t
  and entry = { part_view : part_view; children : tree Snoc_list.t }
end =
  T

and Env : sig
  type t

  val empty : t
  val lookup : t -> id:Id.t -> T.value option
  val extend : t -> id:Id.t -> value:T.value -> t
end = struct
  type t = T.value Id.Map.t

  let empty = Id.Map.empty
  let lookup env ~id = Map.find env id
  let extend env ~id ~value = Map.set env ~key:id ~data:value
end

and St_store : sig
  type t

  val empty : t
  val lookup : t -> label:Label.t -> T.value * Job_q.t
  val update : t -> label:Label.t -> value:T.value * Job_q.t -> t
end = struct
  type t = (T.value * Job_q.t) Label.Map.t

  let empty = Label.Map.empty
  let lookup store ~label = Map.find_exn store label
  let update store ~label ~value = Map.set store ~key:label ~data:value
end

and Job_q : (Batched_queue.S with type elt := T.clos) = Batched_queue.M (struct
  type t = T.clos
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
end = struct
  type t = entry Path.Map.t

  let empty = Path.Map.empty

  let lookup_st tree_mem ~path ~label =
    let { part_view; _ } = Map.find_exn tree_mem path in
    match part_view with
    | Root -> failwith "lookup_st: Root"
    | Node { st_store; _ } -> St_store.lookup st_store ~label

  let update_st tree_mem ~path ~label (v, q) =
    let ({ part_view; _ } as entry) = Map.find_exn tree_mem path in
    match part_view with
    | Root -> failwith "update_st: Root"
    | Node ({ st_store; _ } as node) ->
        let st_store = St_store.update st_store ~label ~value:(v, q) in
        Map.set tree_mem ~key:path
          ~data:{ entry with part_view = Node { node with st_store } }

  let get_dec tree_mem ~path =
    let { part_view; _ } = Map.find_exn tree_mem path in
    match part_view with
    | Root -> failwith "set_dec: Root"
    | Node { dec; _ } -> dec

  let set_dec tree_mem ~path dec =
    let ({ part_view; _ } as entry) = Map.find_exn tree_mem path in
    match part_view with
    | Root -> failwith "set_dec: Root"
    | Node node ->
        Map.set tree_mem ~key:path
          ~data:{ entry with part_view = Node { node with dec } }

  let enq_eff tree_mem ~path clos =
    let ({ part_view; _ } as entry) = Map.find_exn tree_mem path in
    match part_view with
    | Root -> failwith "enq_eff: Root"
    | Node ({ eff_q; _ } as node) ->
        let eff_q = Job_q.enqueue eff_q clos in
        Map.set tree_mem ~key:path
          ~data:{ entry with part_view = Node { node with eff_q } }

  let alloc_pt = Map.length
  let lookup_ent tree_mem ~path = Map.find_exn tree_mem path
  let update_ent tree_mem ~path ent = Map.set tree_mem ~key:path ~data:ent
end

module Value = struct
  type t = value

  let equal v1 v2 =
    match (v1, v2) with
    | Unit, Unit -> true
    | Int i1, Int i2 -> i1 = i2
    | _, _ -> false

  let ( = ) = equal
  let ( <> ) v1 v2 = not (v1 = v2)
  let to_int = function Int i -> Some i | _ -> None

  let to_vs = function
    | Unit -> Some Vs_null
    | Int i -> Some (Vs_int i)
    | Comp_thunk t -> Some (Vs_comp t)
    | _ -> None

  let to_vss = function View_spec vss -> Some vss | _ -> None
  let to_clos = function Clos c -> Some c | _ -> None
end

module Phase = struct
  type t = phase

  let equal p1 p2 =
    match (p1, p2) with
    | P_init, P_init
    | P_update, P_update
    | P_retry, P_retry
    | P_effect, P_effect
    | P_top, P_top ->
        true
    | _, _ -> false

  let ( = ) = equal
  let ( <> ) p1 p2 = not (p1 = p2)
end
