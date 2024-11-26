open! Core
open Syntax
open Concrete_domains

type event =
  | Evt_update_st of (Path.t * Label.t * (value * Job_q.t))
  | Evt_set_dec of (Path.t * decision)
  | Evt_enq_eff of (Path.t * clos)
  | Evt_alloc_pt of Path.t

val record : event -> unit

type diagnostics

val diagnostics : diagnostics ref
