open Stdlib.Effect
open Stdlib.Effect.Deep
open React_trace
include Recorder_intf

(* TODO: Replace the dummy string with an actual recording type *)
type recording = string

let emp_recording = "empty recording"

let event_h =
  {
    retc = (fun v ~recording -> (v, recording));
    exnc = raise;
    effc =
      (fun (type a) (eff : a t) ->
        match eff with
        | Evt_update_st (path, label, (v, q)) ->
            Some
              (fun (k : (a, _) continuation) ~(recording : recording) ->
                ignore (path, label, v, q);
                continue k () ~recording)
        | Evt_set_dec (path, dec) ->
            Some
              (fun (k : (a, _) continuation) ~(recording : recording) ->
                ignore (path, dec);
                continue k () ~recording)
        | Evt_enq_eff (path, clos) ->
            Some
              (fun (k : (a, _) continuation) ~(recording : recording) ->
                ignore (path, clos);
                continue k () ~recording)
        | Evt_alloc_pt path ->
            Some
              (fun (k : (a, _) continuation) ~(recording : recording) ->
                ignore path;
                continue k () ~recording)
        | _ -> None);
  }
