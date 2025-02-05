open! Base
open Lib_domains
open Syntax
open Concrete_domains

exception Unbound_var of string
exception Type_error
exception Invalid_phase

(* path and phase effects *)
type _ eff += Rd_pt : Path.t eff | Rd_ph : phase eff

(* environmental effects *)
type _ eff +=
  | Rd_env : Env.t eff
  | In_env : Env.t -> (('b -> 'a) -> 'b -> 'a) eff

(* memory effects *)
type _ eff +=
  | Alloc_addr : obj -> Addr.t eff
  | Lookup_addr : Addr.t -> obj eff
  | Update_addr : Addr.t * obj -> unit eff

(* tree memory effects in eval/eval_mult *)
type _ eff +=
  | Lookup_st : Path.t * Label.t -> (value * Job_q.t) eff
  | Update_st : (Path.t * Label.t * (value * Job_q.t)) -> unit eff
  | Get_dec : Path.t -> decision eff
  | Set_dec : Path.t * decision -> unit eff
  | Enq_eff : Path.t * clos -> unit eff

(* tree memory effects in render *)
type _ eff +=
  | Alloc_pt : Path.t eff
  | Lookup_ent : Path.t -> entry eff
  | Update_ent : Path.t * entry -> unit eff

(* tree memory effects for instrumentation *)
type _ eff += Get_root_pt : Path.t eff

type checkpoint =
  | Retry_start of (int * Path.t)
  | Render_check of Path.t
  | Render_finish of Path.t
  | Effects_finish of Path.t

type _ eff += Checkpoint : { msg : string; checkpoint : checkpoint } -> unit eff

(* For testing nontermination *)
type _ eff += Re_render_limit : int eff

exception Too_many_re_renders
