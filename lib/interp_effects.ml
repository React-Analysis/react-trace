open! Base
open Stdlib.Effect
open Syntax
open Concrete_domains

exception Unbound_var of string
exception Type_error
exception Invalid_phase

(* path and phase effects *)
type _ Stdlib.Effect.t += Rd_pt : Path.t t | Rd_ph : phase t

(* environmental effects *)
type _ Stdlib.Effect.t +=
  | Rd_env : Env.t t
  | In_env : Env.t -> (('b -> 'a) -> 'b -> 'a) t

(* memory effects *)
type _ Stdlib.Effect.t +=
  | Alloc_addr : obj -> Addr.t t
  | Lookup_addr : Addr.t -> obj t
  | Update_addr : Addr.t * obj -> unit t

(* tree memory effects in eval/eval_mult *)
type _ Stdlib.Effect.t +=
  | Lookup_st : Path.t * Label.t -> (value * Job_q.t) t
  | Update_st : (Path.t * Label.t * (value * Job_q.t)) -> unit t
  | Get_dec : Path.t -> decision t
  | Set_dec : Path.t * decision -> unit t
  | Enq_eff : Path.t * clos -> unit t

(* tree memory effects in render *)
type _ Stdlib.Effect.t +=
  | Alloc_pt : Path.t t
  | Lookup_ent : Path.t -> entry t
  | Update_ent : Path.t * entry -> unit t

type checkpoint =
  | Retry_start of (int * Path.t)
  | Render_check of Path.t
  | Render_finish of Path.t
  | Effects_finish of Path.t

type _ Stdlib.Effect.t +=
  | Checkpoint : { msg : string; checkpoint : checkpoint } -> unit t

(* For testing nontermination *)
type _ Stdlib.Effect.t += Re_render_limit : int t

exception Too_many_re_renders
