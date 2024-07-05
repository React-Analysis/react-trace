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

(* For testing nontermination *)
type _ eff += Re_render_limit : int eff

exception Too_many_re_renders

let re_render_limit_h (type a b) (f : b -> a) (x : b) : re_render_limit:int -> a
    =
  match f x with
  | v -> fun ~re_render_limit:_ -> v
  | effect Re_render_limit, k ->
      fun ~re_render_limit -> continue k re_render_limit ~re_render_limit

let ptph_h (type a b) (f : b -> a) (x : b) : ptph:Path.t * phase -> a =
  match f x with
  | v ->
      fun ~ptph ->
        Logger.ptph ptph `Ret;
        v
  | effect Rd_pt, k ->
      fun ~ptph ->
        Logger.ptph ptph `Rd_pt;
        continue k (fst ptph) ~ptph
  | effect Rd_ph, k ->
      fun ~ptph ->
        Logger.ptph ptph `Rd_ph;
        continue k (snd ptph) ~ptph

let rec env_h : type b a. (b -> a) -> b -> env:Env.t -> a =
 fun f x ->
  match f x with
  | v ->
      fun ~env ->
        Logger.env env `Ret;
        v
  | effect Rd_env, k ->
      fun ~env ->
        Logger.env env `Rd_env;
        continue k env ~env
  | effect In_env env', k ->
      fun ~env ->
        Logger.env env (`In_env env');
        continue k (env_h ~env:env') ~env

let mem_h (type a b) (f : b -> a) (x : b) : mem:Tree_mem.t -> a * Tree_mem.t =
  match f x with
  | v ->
      fun ~mem ->
        Logger.mem mem `Ret;
        (v, mem)
  (* in eval *)
  | effect Lookup_st (path, label), k ->
      fun ~mem ->
        Logger.mem mem (`Lookup_st (path, label));
        continue k (Tree_mem.lookup_st mem ~path ~label) ~mem
  | effect Update_st (path, label, (v, q)), k ->
      fun ~mem ->
        Logger.mem mem (`Update_st (path, label, (v, q)));
        continue k () ~mem:(Tree_mem.update_st mem ~path ~label (v, q))
  | effect Get_dec path, k ->
      fun ~mem ->
        Logger.mem mem (`Get_dec path);
        continue k (Tree_mem.get_dec mem ~path) ~mem
  | effect Set_dec (path, dec), k ->
      fun ~mem ->
        Logger.mem mem (`Set_dec (path, dec));
        continue k () ~mem:(Tree_mem.set_dec mem ~path dec)
  | effect Enq_eff (path, clos), k ->
      fun ~mem ->
        Logger.mem mem (`Enq_eff (path, clos));
        continue k () ~mem:(Tree_mem.enq_eff mem ~path clos)
  (* in render *)
  | effect Alloc_pt, k ->
      fun ~mem ->
        Logger.mem mem `Alloc_pt;
        continue k (Tree_mem.alloc_pt mem) ~mem
  | effect Lookup_ent path, k ->
      fun ~mem ->
        Logger.mem mem (`Lookup_ent path);
        continue k (Tree_mem.lookup_ent mem ~path) ~mem
  | effect Update_ent (path, ent), k ->
      fun ~mem ->
        Logger.mem mem (`Update_ent (path, ent));
        continue k () ~mem:(Tree_mem.update_ent mem ~path ent)

let value_exn exn v =
  Option.value_exn v ~error:(Error.of_exn exn ~backtrace:`Get)

let int_of_value_exn v = v |> Value.to_int |> value_exn Type_error
let bool_of_value_exn v = v |> Value.to_bool |> value_exn Type_error
let vs_of_value_exn v = v |> Value.to_vs |> value_exn Type_error
let vss_of_value_exn v = v |> Value.to_vss |> value_exn Type_error
let clos_of_value_exn v = v |> Value.to_clos |> value_exn Type_error

module Env = struct
  include Env

  let lookup_exn env ~id = lookup env ~id |> value_exn (Unbound_var id)
end

let rec eval : type a. a Expr.t -> value =
 fun expr ->
  Logger.eval expr;
  match expr with
  | Const Unit -> Unit
  | Const (Bool b) -> Bool b
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

          let dec =
            if Int.(path = self_pt) && Phase.(phase <> P_effect) then Retry
            else Update
          in
          perform (Set_dec (path, dec));

          (*if Int.(path = self_pt) && Phase.(phase <> P_effect) then*)
          (*  perform (Set_dec (path, Retry));*)
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
      | P_effect -> raise Invalid_phase)
  | Eff e ->
      let path = perform Rd_pt
      and phase = perform Rd_ph
      and env = perform Rd_env in
      (match phase with P_effect -> raise Invalid_phase | _ -> ());
      perform (Enq_eff (path, { param = Id.unit; body = e; env }));
      Unit
  | Seq (e1, e2) ->
      eval e1 |> ignore;
      eval e2
  | Uop { op; arg } -> (
      let v = eval arg in
      match (op, v) with
      | Not, Bool b -> Bool (not b)
      | Uminus, Int i -> Int ~-i
      | _, _ -> raise Type_error)
  | Bop { op; left; right } -> (
      let v1 = eval left in
      let v2 = eval right in
      match (op, v1, v2) with
      | And, Bool b1, Bool b2 -> Bool (b1 && b2)
      | Or, Bool b1, Bool b2 -> Bool (b1 || b2)
      | Plus, Int i1, Int i2 -> Int (i1 + i2)
      | Minus, Int i1, Int i2 -> Int (i1 - i2)
      | Times, Int i1, Int i2 -> Int (i1 * i2)
      | _, _, _ -> raise Type_error)

let rec eval_mult : type a. ?re_render:int -> a Expr.t -> value =
 fun ?(re_render = 1) expr ->
  Logger.eval_mult expr;

  (* This is a hack only used for testing non-termination. *)
  (try if re_render >= perform Re_render_limit then raise Too_many_re_renders
   with Stdlib.Effect.Unhandled Re_render_limit -> ());

  let v = eval expr in
  let path = perform Rd_pt in
  match perform (Get_dec path) with
  | Retry ->
      ptph_h (eval_mult ~re_render:(re_render + 1)) expr ~ptph:(path, P_retry)
  | Idle | Update -> v

let rec render (path : Path.t) (vss : view_spec list) : unit =
  Logger.render path vss;
  List.iter vss ~f:(fun vs ->
      let t = render1 vs in
      (* refetch the whole entry, as the children may have updated the parent *)
      let ({ children; _ } as ent) = perform (Lookup_ent path) in
      perform
        (Update_ent (path, { ent with children = Snoc_list.(children ||> t) })))

and render1 (vs : view_spec) : tree =
  Logger.render1 vs;
  match vs with
  | Vs_null -> Leaf_null
  | Vs_int i -> Leaf_int i
  | Vs_comp ({ comp = { param; body; _ }; env; arg } as comp_spec) ->
      let path = perform Alloc_pt
      and env = Env.extend env ~id:param ~value:arg
      and part_view =
        Node
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

let rec update (path : Path.t) : bool =
  Logger.update path;
  let { part_view; children } = perform (Lookup_ent path) in
  match part_view with
  | Root ->
      Snoc_list.fold children ~init:false ~f:(fun acc t -> acc || update1 t)
  | Node { comp_spec = { comp = { param; body; _ }; env; arg }; dec; _ } -> (
      match dec with
      | Retry -> assert false
      | Idle ->
          Snoc_list.fold children ~init:false ~f:(fun acc t -> acc || update1 t)
      | Update ->
          perform (Set_dec (path, Idle));
          let env = Env.extend env ~id:param ~value:arg in
          let vss =
            (eval_mult |> env_h ~env |> ptph_h ~ptph:(path, P_update)) body
            |> vss_of_value_exn
          in

          let old_trees =
            children |> Snoc_list.to_list
            |> Util.pad_or_truncate ~len:(List.length vss)
          in
          (* TODO: We assume that updates from a younger sibling to an older
             sibling are not dropped, while those from an older sibling to a
             younger sibling are. That's why we are resetting the children list
             and then snoc each child again in the reconcile function. We should
             verify this behavior. *)
          let ent = perform (Lookup_ent path) in
          perform (Update_ent (path, { ent with children = [] }));
          reconcile path old_trees vss;
          let dec = perform (Get_dec path) in
          Decision.(dec <> Idle))

and update1 (t : tree) : bool =
  Logger.update1 t;
  match t with Leaf_null | Leaf_int _ -> false | Path path -> update path

and reconcile (path : Path.t) (old_trees : tree option list)
    (vss : view_spec list) : unit =
  Logger.reconcile path old_trees vss;
  List.iter2_exn old_trees vss ~f:(fun old_tree vs ->
      let t = reconcile1 old_tree vs in
      let ({ children; _ } as ent) = perform (Lookup_ent path) in
      perform
        (Update_ent (path, { ent with children = Snoc_list.(children ||> t) })))

and reconcile1 (old_tree : tree option) (vs : view_spec) : tree =
  Logger.reconcile1 old_tree vs;
  match (old_tree, vs) with
  | Some (Leaf_null as t), Vs_null -> t
  | Some (Leaf_int i as t), Vs_int j when i = j -> t
  | Some (Path pt as t), (Vs_comp { comp = { name; _ }; arg; _ } as vs) -> (
      let { part_view; _ } = perform (Lookup_ent pt) in
      match part_view with
      | Root -> assert false
      | Node { comp_spec = { comp = { name = name'; _ }; arg = arg'; _ }; _ } ->
          if Id.(name = name') && Value.(arg = arg') then t else render1 vs)
  | _, vs -> render1 vs

let rec commit_effs (path : Path.t) : unit =
  Logger.commit_effs path;
  let { part_view; children } = perform (Lookup_ent path) in
  (match part_view with
  | Root -> ()
  | Node { eff_q; _ } -> (
      Job_q.iter eff_q ~f:(fun { body; env; _ } ->
          (eval |> env_h ~env |> ptph_h ~ptph:(path, P_effect)) body |> ignore);

      (* Refetch the entry, as committing effects may change the entry *)
      let ent = perform (Lookup_ent path) in
      match ent.part_view with
      | Root -> assert false
      | Node node ->
          perform
            (Update_ent
               ( path,
                 { ent with part_view = Node { node with eff_q = Job_q.empty } }
               ))));
  Snoc_list.iter children ~f:commit_effs1

and commit_effs1 (t : tree) : unit =
  Logger.commit_effs1 t;
  match t with Leaf_null | Leaf_int _ -> () | Path path -> commit_effs path

let rec eval_top (prog : Prog.t) : view_spec list =
  Logger.eval_top prog;
  match prog with
  | Expr e -> eval e |> vss_of_value_exn
  | Comp (comp, p) ->
      let env = perform Rd_env in
      let env = Env.extend env ~id:comp.name ~value:(Comp_clos { comp; env }) in
      perform (In_env env) eval_top p

let step_prog (prog : Prog.t) : Path.t =
  Logger.step_prog prog;
  let vss = env_h eval_top prog ~env:Env.empty in
  let path = perform Alloc_pt in
  perform (Update_ent (path, { part_view = Root; children = [] }));
  render path vss;
  commit_effs path;
  path

let step_path (path : Path.t) : bool =
  Logger.step_path path;
  let has_updates = update path in
  if has_updates then commit_effs path;
  has_updates

type run_info = { steps : int }

let run ?(fuel : int option) (prog : Prog.t) : run_info =
  Logger.run prog;
  let driver () =
    let cnt = ref 1 in
    let path = step_prog prog in
    let rec loop () =
      Int.incr cnt;
      Logs.info (fun m -> m "Step %d" !cnt);
      if step_path path then
        match fuel with Some n when !cnt >= n -> () | _ -> loop ()
    in
    loop ();
    !cnt
  in
  let steps = mem_h driver () ~mem:Tree_mem.empty |> fst in
  { steps }
