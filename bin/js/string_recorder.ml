open! Base
open Stdlib.Effect
open Stdlib.Effect.Deep
open React_trace
open Concrete_domains
open Interp_effects
include Recorder_intf

type recording = string

let emp_recording = "= Recording =\n"

let event_h =
  {
    retc = (fun v ~recording -> (v, recording));
    exnc = raise;
    effc =
      (fun (type a) (eff : a t) ->
        match eff with
        | Update_st (path, label, (v, q)) ->
            Some
              (fun (k : (a, _) continuation) ~(recording : recording) ->
                let () = perform (Update_st (path, label, (v, q))) in
                let recording =
                  recording
                  ^ Printf.sprintf "[path %s] Update state %d -> %s\n"
                      (Sexp.to_string (Path.sexp_of_t path))
                      label
                      (Sexp.to_string (sexp_of_value v))
                in
                continue k () ~recording)
        | Set_dec (path, dec) ->
            Some
              (fun (k : (a, _) continuation) ~(recording : recording) ->
                let () = perform (Set_dec (path, dec)) in
                let recording =
                  recording
                  ^ Printf.sprintf "[path %s] Set decision %s\n"
                      (Sexp.to_string (Path.sexp_of_t path))
                      (Sexp.to_string (sexp_of_decision dec))
                in
                continue k () ~recording)
        | Enq_eff (path, clos) ->
            Some
              (fun (k : (a, _) continuation) ~(recording : recording) ->
                let () = perform (Enq_eff (path, clos)) in
                let recording =
                  recording
                  ^ Printf.sprintf "[path %s] Enqueue effect\n"
                      (Sexp.to_string (Path.sexp_of_t path))
                in
                continue k () ~recording)
        | Alloc_pt ->
            Some
              (fun (k : (a, _) continuation) ~(recording : recording) ->
                let path = perform Alloc_pt in
                let recording =
                  recording
                  ^ Printf.sprintf "Allocate path %s\n"
                      (Sexp.to_string (Path.sexp_of_t path))
                in
                continue k path ~recording)
        | _ -> None);
  }
