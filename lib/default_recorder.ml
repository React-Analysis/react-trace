open Stdlib.Effect
open Stdlib.Effect.Deep
open Interp_effects
include Recorder_intf

type recording = unit

let emp_recording = ()

let event_h (type a b) (f : a -> b) (x : a) :
    recording:recording -> b * recording =
  match f x with
  | v -> fun ~recording -> (v, recording)
  | effect Update_st (path, label, (v, q)), k ->
      fun ~recording ->
        let () = perform (Update_st (path, label, (v, q))) in
        continue k () ~recording
  | effect Set_dec (path, dec), k ->
      fun ~recording ->
        let () = perform (Set_dec (path, dec)) in
        continue k () ~recording
  | effect Enq_eff (path, clos), k ->
      fun ~recording ->
        let () = perform (Enq_eff (path, clos)) in
        continue k () ~recording
  | effect Alloc_pt, k ->
      fun ~recording ->
        let path = perform Alloc_pt in
        continue k path ~recording
  | effect Checkpoint _, k -> fun ~recording -> continue k () ~recording
