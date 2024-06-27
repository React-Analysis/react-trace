open! Base
open Stdlib.Effect
open Stdlib.Effect.Deep
open Syntax
open Domains

exception Unbound_var of string
exception Type_error
exception Invalid_phase

(* path and phase effects *)
type _ eff += Rd_pt : Path.t eff | Rd_ph : phase eff

(* environmental effects *)
type _ eff +=
  | Rd_env : Env.t eff
  | In_env : Env.t -> (('b -> 'a) -> 'b -> 'a) eff

(* memory effects in eval/eval_mult *)
type _ eff +=
  | Lookup_st : Path.t * Label.t -> (value * Job_q.t) eff
  | Update_st : (Path.t * Label.t * (value * Job_q.t)) -> unit eff
  | Get_dec : Path.t -> decision eff
  | Set_dec : Path.t * decision -> unit eff
  | Enq_eff : Path.t * clos -> unit eff

(* memory effects in render *)
type _ eff +=
  | Alloc_pt : Path.t eff
  | Lookup_ent : Path.t -> entry eff
  | Update_ent : Path.t * entry -> unit eff

let ptph_h (type a b) (f : b -> a) (x : b) : ptph:Path.t * phase -> a =
  match f x with
  | v -> fun ~ptph:_ -> v
  | effect Rd_pt, k -> fun ~ptph -> continue k (fst ptph) ~ptph
  | effect Rd_ph, k -> fun ~ptph -> continue k (snd ptph) ~ptph

let rec env_h : type a b. (b -> a) -> b -> env:Env.t -> a =
 fun f x ->
  match f x with
  | v -> fun ~env:_ -> v
  | effect Rd_env, k -> fun ~env -> continue k env ~env
  | effect In_env env', k -> fun ~env -> continue k (env_h ~env:env') ~env

let mem_h (type a b) (f : b -> a) (x : b) : mem:Tree_mem.t -> a * Tree_mem.t =
  match f x with
  | v -> fun ~mem -> (v, mem)
  (* in eval *)
  | effect Lookup_st (path, label), k ->
      fun ~mem -> continue k (Tree_mem.lookup_st mem ~path ~label) ~mem
  | effect Update_st (path, label, (v, q)), k ->
      fun ~mem ->
        continue k () ~mem:(Tree_mem.update_st mem ~path ~label (v, q))
  | effect Get_dec path, k ->
      fun ~mem -> continue k (Tree_mem.get_dec mem ~path) ~mem
  | effect Set_dec (path, dec), k ->
      fun ~mem -> continue k () ~mem:(Tree_mem.set_dec mem ~path dec)
  | effect Enq_eff (path, clos), k ->
      fun ~mem -> continue k () ~mem:(Tree_mem.enq_eff mem ~path clos)
  (* in render *)
  | effect Alloc_pt, k -> fun ~mem -> continue k (Tree_mem.alloc_pt mem) ~mem
  | effect Lookup_ent path, k ->
      fun ~mem -> continue k (Tree_mem.lookup_ent mem ~path) ~mem
  | effect Update_ent (path, ent), k ->
      fun ~mem -> continue k () ~mem:(Tree_mem.update_ent mem ~path ent)

let value_exn exn v =
  Option.value_exn v ~error:(Error.of_exn exn ~backtrace:`Get)

let int_of_value_exn v = v |> Value.to_int |> value_exn Type_error
let vs_of_value_exn v = v |> Value.to_vs |> value_exn Type_error
let vss_of_value_exn v = v |> Value.to_vss |> value_exn Type_error
let clos_of_value_exn v = v |> Value.to_clos |> value_exn Type_error

module Env = struct
  include Env

  let lookup_exn env ~id = lookup env ~id |> value_exn (Unbound_var id)
end

let rec eval : type a. a Expr.t -> value = function
  | Const (Unit ()) -> Unit
  | Const (Int i) -> Int i
  | Var id ->
      let env = perform Rd_env in
      Env.lookup_exn env ~id
  | View es -> View_spec (List.map es ~f:(fun e -> eval e |> vs_of_value_exn))
  | Cond { pred; con; alt } ->
      let p = eval pred |> int_of_value_exn in
      if Int.(p <> 0) then eval con else eval alt
  | Fn { param; body } -> Clos { param; body; env = perform Rd_env }
  | App { fn; arg } -> (
      match eval fn with
      | Clos { param; body; env } ->
          let env = Env.extend env ~id:param ~value:(eval arg) in
          perform (In_env env) eval body
      | Comp_clos { comp; env } -> Comp_spec { comp; env; arg = eval arg }
      | Set_clos { label; path } ->
          (* Argument to the setter should be a setting thunk *)
          let clos = eval arg |> clos_of_value_exn in

          let self_pt = perform Rd_pt in
          let phase = perform Rd_ph in
          if Phase.(phase = P_top) then assert false;

          let dec =
            if Int.(path = self_pt) && Phase.(phase <> P_effect) then Retry
            else Update
          in
          perform (Set_dec (path, dec));

          let v, q = perform (Lookup_st (path, label)) in
          perform (Update_st (path, label, (v, Job_q.enqueue q clos)));

          Unit
      | _ -> raise Type_error)
  | Let { id; bound; body } ->
      let value = eval bound in
      let env = Env.extend (perform Rd_env) ~id ~value in
      perform (In_env env) eval body
  | Stt { label; stt; set; init; body } -> (
      let path = perform Rd_pt in
      match perform Rd_ph with
      | P_init ->
          let v = eval init in
          let env =
            perform Rd_env
            |> Env.extend ~id:stt ~value:v
            |> Env.extend ~id:set ~value:(Set_clos { label; path })
          in
          perform (Update_st (path, label, (v, Job_q.empty)));
          perform (In_env env) eval body
      | P_update | P_retry ->
          let v_old, q = perform (Lookup_st (path, label)) in
          (* Run the setting thunks in the set queue *)
          let v =
            Job_q.fold q ~init:v_old ~f:(fun value { param; body; env } ->
                let env = Env.extend env ~id:param ~value in
                perform (In_env env) eval body)
          in

          let env =
            perform Rd_env
            |> Env.extend ~id:stt ~value:v
            |> Env.extend ~id:set ~value:(Set_clos { label; path })
          in
          if Value.(v_old <> v) then perform (Set_dec (path, Update));
          perform (Update_st (path, label, (v, Job_q.empty)));
          perform (In_env env) eval body
      | P_effect | P_top -> raise Invalid_phase)
  | Eff e ->
      let path = perform Rd_pt
      and phase = perform Rd_ph
      and env = perform Rd_env in
      (match phase with P_effect | P_top -> raise Invalid_phase | _ -> ());
      perform (Enq_eff (path, { param = Id.unit; body = e; env }));
      Unit
  | Seq (e1, e2) ->
      eval e1 |> ignore;
      eval e2
  | Bin_op { op; left; right } -> (
      let v1 = eval left in
      let v2 = eval right in
      match (op, v1, v2) with
      | Plus, Int i1, Int i2 -> Int (i1 + i2)
      | Minus, Int i1, Int i2 -> Int (i1 - i2)
      | Times, Int i1, Int i2 -> Int (i1 * i2)
      | _, _, _ -> raise Type_error)

let rec eval_mult : type a. a Expr.t -> value =
 fun expr ->
  let v = eval expr in
  let path = perform Rd_pt in
  match perform (Get_dec path) with
  | Retry -> ptph_h eval_mult expr ~ptph:(path, P_retry)
  | Idle | Update -> v

let rec render (path : Path.t) (vss : view_spec list) : unit =
  List.iter vss ~f:(fun vs ->
      let t = render1 vs in
      (* refetch the whole entry, as the children may have updated the parent *)
      let ({ children; _ } as ent) = perform (Lookup_ent path) in
      perform
        (Update_ent (path, { ent with children = Snoc_list.(children ||> t) })))

and render1 : view_spec -> tree = function
  | Vs_null -> Leaf_null
  | Vs_int i -> Leaf_int i
  | Vs_comp ({ comp = { param; body; _ }; env; arg } as comp_spec) ->
      let path = perform Alloc_pt
      and env = Env.extend env ~id:param ~value:arg
      and part_view =
        {
          comp_spec;
          dec = Idle;
          st_store = St_store.empty;
          eff_q = Job_q.empty;
        }
      in
      perform (Update_ent (path, { part_view; children = [] }));

      let vss =
        (eval_mult |> env_h ~env |> ptph_h ~ptph:(path, P_init)) body
        |> vss_of_value_exn
      in
      render path vss;

      Path path

let rec update (path : Path.t) : unit =
  let {
    part_view = { comp_spec = { comp = { param; body; _ }; env; arg }; dec; _ };
    children;
  } =
    perform (Lookup_ent path)
  in
  match dec with
  | Retry -> assert false
  | Idle -> Snoc_list.iter children ~f:update1
  | Update ->
      let env = Env.extend env ~id:param ~value:arg in
      let vss =
        (eval_mult |> env_h ~env |> ptph_h ~ptph:(path, P_update)) body
        |> vss_of_value_exn
      in

      let old_trees =
        children |> Snoc_list.to_list
        |> Util.pad_or_truncate ~len:(List.length vss)
      in
      (* TODO: We assume that updates from a younger sibling to an older sibling
         are not dropped, while those from an older sibling to a younger sibling
         are. That's why we are resetting the children list and then snoc each
         child again in the reconcile function. We should verify this
         behavior. *)
      let ent = perform (Lookup_ent path) in
      perform (Update_ent (path, { ent with children = [] }));
      reconcile path old_trees vss

and update1 : tree -> unit = function
  | Leaf_null | Leaf_int _ -> ()
  | Path path -> update path

and reconcile (path : Path.t) (old_trees : tree option list)
    (vss : view_spec list) : unit =
  List.iter2_exn old_trees vss ~f:(fun old_tree new_vs ->
      let t =
        match (old_tree, new_vs) with
        | Some (Leaf_null as t), Vs_null -> t
        | Some (Leaf_int i as t), Vs_int j when i = j -> t
        | Some (Path pt as t), (Vs_comp { comp = { name; _ }; arg; _ } as vs) ->
            let {
              part_view =
                { comp_spec = { comp = { name = name'; _ }; arg = arg'; _ }; _ };
              _;
            } =
              perform (Lookup_ent pt)
            in
            if Id.(name = name') && Value.(arg = arg') then (
              update1 t;
              t)
            else render1 vs
        | _, vs -> render1 vs
      in
      let ({ children; _ } as ent) = perform (Lookup_ent path) in
      perform
        (Update_ent (path, { ent with children = Snoc_list.(children ||> t) })))

let rec commit_effs (path : Path.t) : unit =
  let { part_view; children } = perform (Lookup_ent path) in
  Job_q.iter part_view.eff_q ~f:(fun { body; env; _ } ->
      env_h eval body ~env |> ignore);

  Snoc_list.iter children ~f:commit_effs1

and commit_effs1 : tree -> unit = function
  | Leaf_null | Leaf_int _ -> ()
  | Path path -> commit_effs path
