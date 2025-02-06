open! Base
open Stdlib.Effect
open Stdlib.Effect.Deep
open React_trace
open Interp_effects
open Lib_domains
open Concrete_domains
include Recorder_intf
module B = PrintBox

type recording = (string * B.t) list

let align ?(h = `Center) ?(v = `Center) = B.align ~h ~v
let bold_text = B.(text_with_style Style.bold)

let trunc ?(max_len = 10) s =
  if String.length s > max_len then String.prefix s max_len ^ "…" else s

let value (v : value) : B.t =
  sexp_of_value v |> Sexp.to_string |> trunc |> B.text

let clos ({ param; _ } : clos) : B.t = "λ" ^ param ^ ".<body>" |> B.text
let leaf_null () : B.t = B.text "()"
let leaf_int (i : int) : B.t = B.int i

let rec tree : tree -> B.t = function
  | Leaf_null -> leaf_null ()
  | Leaf_int i -> leaf_int i
  | Path p -> path p

and path (pt : Path.t) : B.t =
  let { part_view; children } = perform (Lookup_ent pt) in
  let part_view_box =
    match part_view with
    | Root -> bold_text "•" |> align
    | Node { comp_spec = { comp; arg; _ }; dec; st_store; eff_q } ->
        let comp_spec_box =
          B.(
            hlist ~bars:false
              [ bold_text (trunc comp.name); text " "; value arg ])
          |> align
        in
        let dec_box =
          let dec = sexp_of_decision dec |> Sexp.to_string in
          B.(hlist_map text [ "dec"; dec ])
        in
        let stt_box =
          let st_trees =
            let st_store = St_store.to_alist st_store in
            List.map st_store ~f:(fun (lbl, (value, job_q)) ->
                let lbl = Int.to_string lbl in
                let value = Sexp.to_string (sexp_of_value value) in
                let job_q = Job_q.to_list job_q |> List.map ~f:clos in

                B.(tree (text (lbl ^ " ↦ " ^ value)) job_q))
            |> B.vlist
          in
          B.(hlist [ text "stt"; st_trees ])
        in
        let eff_box =
          let eff_q = Job_q.to_list eff_q |> List.map ~f:clos in
          B.(hlist [ text "eff"; vlist eff_q ])
        in
        B.vlist [ comp_spec_box; dec_box; stt_box; eff_box ]
  in
  let children =
    Snoc_list.to_list children |> B.hlist_map (fun t -> tree t |> align)
  in
  B.(vlist [ part_view_box; children ] |> frame)

let emp_recording = []

let event_h (type a b) (f : a -> b) (x : a) :
    recording:recording -> b * recording =
  match f x with
  | v -> fun ~recording -> (v, recording)
  | effect Checkpoint { msg; _ }, k ->
      fun ~recording ->
        let root = perform Get_root_pt in
        let box = path root in
        continue k () ~recording:((msg, box) :: recording)
