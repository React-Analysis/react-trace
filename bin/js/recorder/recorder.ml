open! Base
open Stdlib.Effect
open Stdlib.Effect.Deep
open React_trace
open Lib_domains
open Concrete_domains
open Interp_effects
include Recorder_intf

(* TODO: Extract this function to a common module *)
let get_path_from_checkpoint = function
  | Retry_start (_, pt) | Render_check pt | Render_finish pt | Effects_finish pt
    ->
      pt

type entry = { msg : string }
type recording = { checkpoints : entry list; log : string }

let emp_recording = { checkpoints = []; log = "= Recording =\n" }

let event_h (type a b) (f : a -> b) (x : a) :
    recording:recording -> b * recording =
  match f x with
  | v -> fun ~recording -> (v, recording)
  | effect Update_st (path, label, (v, q)), k ->
      fun ~recording ->
        let () = perform (Update_st (path, label, (v, q))) in
        let recording =
          {
            recording with
            log =
              recording.log
              ^ Printf.sprintf "[path %s] Update state %d -> %s\n"
                  (Sexp.to_string (Path.sexp_of_t path))
                  label
                  (Sexp.to_string (sexp_of_value v));
          }
        in
        continue k () ~recording
  | effect Set_dec (path, dec), k ->
      fun ~recording ->
        let () = perform (Set_dec (path, dec)) in
        let recording =
          {
            recording with
            log =
              recording.log
              ^ Printf.sprintf "[path %s] Set decision %s\n"
                  (Sexp.to_string (Path.sexp_of_t path))
                  (Sexp.to_string (sexp_of_decision dec));
          }
        in
        continue k () ~recording
  | effect Enq_eff (path, clos), k ->
      fun ~recording ->
        let () = perform (Enq_eff (path, clos)) in
        let recording =
          {
            recording with
            log =
              recording.log
              ^ Printf.sprintf "[path %s] Enqueue effect\n"
                  (Sexp.to_string (Path.sexp_of_t path));
          }
        in
        continue k () ~recording
  | effect Alloc_pt, k ->
      fun ~recording ->
        let path = perform Alloc_pt in
        let recording =
          {
            recording with
            log =
              recording.log
              ^ Printf.sprintf "Allocate path %s\n"
                  (Sexp.to_string (Path.sexp_of_t path));
          }
        in
        continue k path ~recording
  | effect Checkpoint { msg; checkpoint }, k ->
      fun ~recording ->
        let root = perform Get_root_pt in
        let pt = get_path_from_checkpoint checkpoint in
        let msg =
          Printf.sprintf "[%s/%s] %s"
            (Sexp.to_string (Path.sexp_of_t pt))
            (Sexp.to_string (Path.sexp_of_t root))
            msg
        in
        let recording =
          { recording with checkpoints = { msg } :: recording.checkpoints }
        in
        continue k () ~recording
