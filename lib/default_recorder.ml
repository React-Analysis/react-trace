open Stdlib.Effect
open Stdlib.Effect.Deep
open Interp_effects
include Recorder_intf

type recording = unit

let emp_recording = ()

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
                continue k () ~recording)
        | Set_dec (path, dec) ->
            Some
              (fun (k : (a, _) continuation) ~(recording : recording) ->
                let () = perform (Set_dec (path, dec)) in
                continue k () ~recording)
        | Enq_eff (path, clos) ->
            Some
              (fun (k : (a, _) continuation) ~(recording : recording) ->
                let () = perform (Enq_eff (path, clos)) in
                continue k () ~recording)
        | Alloc_pt ->
            Some
              (fun (k : (a, _) continuation) ~(recording : recording) ->
                let path = perform Alloc_pt in
                continue k path ~recording)
        | Checkpoint _ ->
            Some
              (fun (k : (a, _) continuation) ~(recording : recording) ->
                continue k () ~recording)
        | _ -> None);
  }
