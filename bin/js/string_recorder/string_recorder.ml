open! Base
open Stdlib.Effect
open Stdlib.Effect.Deep
open React_trace
open Lib_domains
open Concrete_domains
open Interp_effects
include Recorder.Recorder_intf

type recording = string

let emp_recording = "= Recording =\n"

let event_h (type a b) (f : a -> b) (x : a) :
    recording:recording -> b * recording =
  match f x with
  | v -> fun ~recording -> (v, recording)
  | effect Update_st (path, label, (v, q)), k ->
      fun ~recording ->
        let () = perform (Update_st (path, label, (v, q))) in
        let recording =
          recording
          ^ Printf.sprintf "[path %s] Update state %d -> %s\n"
              (Sexp.to_string (Path.sexp_of_t path))
              label
              (Sexp.to_string (sexp_of_value v))
        in
        continue k () ~recording
  | effect Set_dec (path, dec), k ->
      fun ~recording ->
        let () = perform (Set_dec (path, dec)) in
        let recording =
          recording
          ^ Printf.sprintf "[path %s] Set decision %s\n"
              (Sexp.to_string (Path.sexp_of_t path))
              (Sexp.to_string (sexp_of_decision dec))
        in
        continue k () ~recording
  | effect Enq_eff (path, clos), k ->
      fun ~recording ->
        let () = perform (Enq_eff (path, clos)) in
        let recording =
          recording
          ^ Printf.sprintf "[path %s] Enqueue effect\n"
              (Sexp.to_string (Path.sexp_of_t path))
        in
        continue k () ~recording
  | effect Alloc_pt, k ->
      fun ~recording ->
        let path = perform Alloc_pt in
        let recording =
          recording
          ^ Printf.sprintf "Allocate path %s\n"
              (Sexp.to_string (Path.sexp_of_t path))
        in
        continue k path ~recording
  | effect Checkpoint _, k -> fun ~recording -> continue k () ~recording
