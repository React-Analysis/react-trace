open! Core
open Syntax

module type T = sig
  type path
  type env
  type addr
  type obj
  type st_store
  type job_q
  type clos = { self : Id.t option; param : Id.t; body : Expr.hook_free_t; env : env }
  type set_clos = { label : Label.t; path : path }
  type comp_clos = { comp : Prog.comp; env : env }

  type value =
    | Unit
    | Bool of bool
    | Int of int
    | String of string
    | Addr of addr
    | View_spec of view_spec list
    | Clos of clos
    | Set_clos of set_clos
    | Comp_clos of comp_clos
    | Comp_spec of comp_spec

  and comp_spec = { comp : Prog.comp; env : env; arg : value }
  and view_spec = Vs_null | Vs_int of int | Vs_comp of comp_spec

  type phase = P_init | P_update | P_retry | P_effect
  type decision = Idle | Retry | Update

  type part_view =
    | Root
    | Node of {
        comp_spec : comp_spec;
        dec : decision;
        st_store : st_store;
        eff_q : job_q;
      }

  type tree = Leaf_null | Leaf_int of int | Path of path
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
  val sexp_of_addr : addr -> Sexp.t
  val sexp_of_obj : obj -> Sexp.t
end

module type Path = sig
  type t
  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
  val sexp_of_t : t -> Sexp.t
  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module type Env = sig
  type value
  type t

  val empty : t
  val lookup : t -> id:Id.t -> value option
  val extend : t -> id:Id.t -> value:value -> t
  val sexp_of_t : t -> Sexp.t
end

module type Addr = sig
  type t
  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
  val sexp_of_t : t -> Sexp.t
  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module type Obj = sig
  type value
  type t

  val empty : t
  val lookup : t -> field:Id.t -> value
  val update : t -> field:Id.t -> value:value -> t
  val sexp_of_t : t -> Sexp.t
end

module type Memory = sig
  type obj
  type addr
  type t

  val empty : t
  val alloc : t -> addr
  val lookup : t -> addr:addr -> obj
  val update : t -> addr:addr -> obj:obj -> t
  val sexp_of_t : t -> Sexp.t
end

module type St_store = sig
  type value
  type job_q
  type t

  val empty : t
  val lookup : t -> label:Label.t -> value * job_q
  val update : t -> label:Label.t -> value:value * job_q -> t
  val to_alist : t -> (Label.t * (value * job_q)) list
  val sexp_of_t : t -> Sexp.t
end

module type Job_q = Batched_queue.S

module type Tree_mem = sig
  type value
  type path
  type job_q
  type decision
  type clos
  type entry
  type t

  val empty : t
  val lookup_st : t -> path:path -> label:Label.t -> value * job_q
  val update_st : t -> path:path -> label:Label.t -> value * job_q -> t
  val get_dec : t -> path:path -> decision
  val set_dec : t -> path:path -> decision -> t
  val enq_eff : t -> path:path -> clos -> t
  val alloc_pt : t -> path
  val lookup_ent : t -> path:path -> entry
  val update_ent : t -> path:path -> entry -> t
  val sexp_of_t : t -> Sexp.t
end

module type Value = sig
  type view_spec
  type clos
  type addr
  type t

  val to_bool : t -> bool option
  val to_int : t -> int option
  val to_string : t -> string option
  val to_addr : t -> addr option
  val to_vs : t -> view_spec option
  val to_vss : t -> view_spec list option
  val to_clos : t -> clos option
  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module type Phase = sig
  type t

  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module type Decision = sig
  type t

  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module type S = sig
  module T : T
  include T
  module Path : Path with type t = path
  module Env : Env with type value = value and type t = env
  module Addr : Addr with type t = addr
  module Obj : Obj with type value = value and type t = obj
  module Memory : Memory with type obj = obj and type addr = addr

  module St_store :
    St_store
      with type value = value
       and type job_q = job_q
       and type t = st_store

  module Job_q : Job_q with type elt := clos and type t = job_q

  module Tree_mem :
    Tree_mem
      with type value = value
       and type path = path
       and type job_q = job_q
       and type decision = decision
       and type clos = clos
       and type entry = entry

  module Value :
    Value
      with type view_spec = view_spec
       and type clos = clos
       and type t = value
       and type addr = addr

  module Phase : Phase with type t = phase
  module Decision : Decision with type t = decision
end
