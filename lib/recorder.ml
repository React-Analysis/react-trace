open! Core
open Syntax
open Concrete_domains
(* open Interp_effects *)

type event =
  | Evt_update_st of (Path.t * Label.t * (value * Job_q.t))
  | Evt_set_dec of (Path.t * decision)
  | Evt_enq_eff of (Path.t * clos)
  | Evt_alloc_pt of Path.t

(** TODO: *)
let record = function
  | Evt_update_st (path, label, (v, q)) -> ignore (path, label, v, q)
  | Evt_set_dec (path, dec) -> ignore (path, dec)
  | Evt_enq_eff (path, clos) -> ignore (path, clos)
  | Evt_alloc_pt path -> ignore path

type diagnostics
(** TODO *)

let diagnostics = ref (failwith "not implemented")
