open! Base
open Stdlib.Effect
open Stdlib.Effect.Deep
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
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

type tree = { path : string; name : string; children : tree list }
and entry = { msg : string; tree : tree }

and recording = { checkpoints : entry list; log : string }
[@@deriving yojson_of]

let emp_recording = { checkpoints = []; log = "" }
let leaf_null () : tree = { path = ""; name = "()"; children = [] }

let leaf_int (i : int) : tree =
  { path = ""; name = Int.to_string i; children = [] }

let rec tree : Concrete_domains.tree -> tree = function
  | Leaf_null -> leaf_null ()
  | Leaf_int i -> leaf_int i
  | Path p -> path p

and path (pt : Path.t) : tree =
  let { part_view; children } = perform (Lookup_ent pt) in
  let name =
    match part_view with
    | Root -> "Root"
    | Node node -> node.comp_spec.comp.name
  in
  {
    path = pt |> Path.sexp_of_t |> Sexp.to_string;
    name;
    children = children |> Snoc_list.to_list |> List.map ~f:tree;
  }

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
        let pt = get_path_from_checkpoint checkpoint in
        let msg =
          Printf.sprintf "[%s] %s" (Sexp.to_string (Path.sexp_of_t pt)) msg
        in
        let root = perform Get_root_pt in
        let tree = path root in
        let recording =
          {
            recording with
            checkpoints = { msg; tree } :: recording.checkpoints;
          }
        in
        continue k () ~recording
