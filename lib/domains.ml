open! Base
open Syntax

module Env = struct
  type 'a t = 'a Id.Map.t

  let empty = Id.Map.empty
  let lookup env ~id = Map.find env id
  let extend env ~id ~value = Map.set env ~key:id ~data:value
end

module Store = struct
  type 'a t = 'a Label.Map.t

  let empty = Label.Map.empty
  let lookup store ~label = Map.find_exn store label
  let update store ~label ~value = Map.set store ~key:label ~data:value
end

module Tree_path = struct
  module T = Int
  include T

  module Map = struct
    open Map
    include M (T)

    let empty = empty (module T)
  end
end

module Job_q = Batched_queue

type value =
  | Unit
  | Int of int
  | View_spec of view_spec list
  | Clos of clos
  | Set_clos of set_clos
  | Comp_clos of comp_clos
  | Comp_thunk of comp_thunk

and clos = { param : Id.t; body : hook_free Expr.t; env : env }
and set_clos = { label : Label.t; path : path }
and comp_clos = { comp : Prog.comp; env : env }
and comp_thunk = { comp : Prog.comp; env : env; arg : value }
and view_spec = Vs_null | Vs_int of int | Vs_comp of comp_thunk
and env = value Env.t
and phase = P_init | P_update | P_retry | P_effect
and node_ctx = { decision : decision; st_store : st_store; tree_mem : tree_mem }
and decision = Idle | Retry | Update
and st_store = (value * job_q) Store.t
and job_q = clos Job_q.t

and part_view = {
  view_spec : view_spec;
  decision : decision;
  st_store : st_store;
  eff_q : job_q;
}

and tree =
  | Leaf of tree_val
  | Node of { part_view : part_view; children : path list }

and tree_val = Leaf_null | Leaf_int of int
and tree_mem = tree option Tree_path.Map.t
and path = Tree_path.t

let empty_env : env = Env.empty
let empty_st_store : st_store = Store.empty
let empty_tree_mem : tree_mem = Tree_path.Map.empty
let empty_setq : job_q = Job_q.empty
let empty_effq : job_q = Job_q.empty

let equal v1 v2 =
  match (v1, v2) with
  | Unit, Unit -> true
  | Int i1, Int i2 -> i1 = i2
  | _, _ -> false

let ( = ) = equal
let ( <> ) v1 v2 = not (equal v1 v2)
