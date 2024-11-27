open! Core
open Stdlib.Effect
open Stdlib.Effect.Deep
open Syntax
open Concrete_domains

type _ Stdlib.Effect.t +=
  | Evt_update_st : (Path.t * Label.t * (value * Job_q.t)) -> unit t
  | Evt_set_dec : (Path.t * decision) -> unit t
  | Evt_enq_eff : (Path.t * clos) -> unit t
  | Evt_alloc_pt : Path.t -> unit t

module type Intf = sig
  type recording

  val emp_recording : recording
  val event_h : 'a. ('a, recording:recording -> 'a * recording) handler
end
