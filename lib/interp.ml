open! Core
open Stdlib.Effect
open Stdlib.Effect.Deep
open Syntax
open Concrete_domains

exception Unbound_var of string
exception Type_error
exception Invalid_phase

(* path and phase effects *)
type _ Stdlib.Effect.t += Rd_pt : Path.t t | Rd_ph : phase t

(* environmental effects *)
type _ Stdlib.Effect.t +=
  | Rd_env : Env.t t
  | In_env : Env.t -> (('b -> 'a) -> 'b -> 'a) t

(* memory effects *)
type _ Stdlib.Effect.t +=
  | Alloc_loc : obj -> Loc.t t
  | Lookup_loc : Loc.t -> obj t
  | Update_loc : Loc.t * obj -> unit t

(* tree memory effects in eval/eval_mult *)
type _ Stdlib.Effect.t +=
  | Lookup_st : Path.t * Label.t -> (value * Job_q.t) t
  | Update_st : (Path.t * Label.t * (value * Job_q.t)) -> unit t
  | Get_dec : Path.t -> decision t
  | Set_dec : Path.t * decision -> unit t
  | Enq_eff : Path.t * clos -> unit t

(* tree memory effects in render *)
type _ Stdlib.Effect.t +=
  | Alloc_pt : Path.t t
  | Lookup_ent : Path.t -> entry t
  | Update_ent : Path.t * entry -> unit t

(* For testing nontermination *)
type _ Stdlib.Effect.t += Re_render_limit : int t

exception Too_many_re_renders

let re_render_limit_h : 'a. ('a, re_render_limit:int -> 'a) handler =
  {
    retc = (fun v ~re_render_limit:_ -> v);
    exnc = raise;
    effc =
      (fun (type b) (eff : b t) ->
        match eff with
        | Re_render_limit ->
            Some
              (fun (k : (b, _) continuation) ~(re_render_limit : int) ->
                continue k re_render_limit ~re_render_limit)
        | _ -> None);
  }

let ptph_h =
  {
    retc =
      (fun v ~ptph ->
        Logger.ptph ptph `Ret;
        v);
    exnc = raise;
    effc =
      (fun (type a) (eff : a t) ->
        match eff with
        | Rd_pt ->
            Some
              (fun (k : (a, _) continuation) ~(ptph : Path.t * phase) ->
                Logger.ptph ptph `Rd_pt;
                continue k (fst ptph) ~ptph)
        | Rd_ph ->
            Some
              (fun (k : (a, _) continuation) ~(ptph : Path.t * phase) ->
                Logger.ptph ptph `Rd_ph;
                continue k (snd ptph) ~ptph)
        | _ -> None);
  }

let rec env_h : 'a. ('a, env:Env.t -> 'a) handler =
  {
    retc =
      (fun v ~env ->
        Logger.env env `Ret;
        v);
    exnc = raise;
    effc =
      (fun (type a) (eff : a t) ->
        match eff with
        | Rd_env ->
            Some
              (fun (k : (a, _) continuation) ~(env : Env.t) ->
                Logger.env env `Rd_env;
                continue k env ~env)
        | In_env env' ->
            Some
              (fun (k : (a, _) continuation) ~(env : Env.t) ->
                Logger.env env (`In_env env');
                continue k (fun f x -> match_with f x env_h ~env:env') ~env)
        | _ -> None);
  }

let mem_h =
  {
    retc =
      (fun v ~mem ->
        Logger.mem mem `Ret;
        (v, mem));
    exnc = raise;
    effc =
      (fun (type a) (eff : a t) ->
        match eff with
        | Alloc_loc obj ->
            Some
              (fun (k : (a, _) continuation) ~(mem : Memory.t) ->
                Logger.mem mem `Alloc_loc;
                let loc = Memory.alloc mem in
                let mem = Memory.update mem ~loc ~obj in
                continue k loc ~mem)
        | Lookup_loc loc ->
            Some
              (fun (k : (a, _) continuation) ~(mem : Memory.t) ->
                Logger.mem mem (`Lookup_loc loc);
                continue k (Memory.lookup mem ~loc) ~mem)
        | Update_loc (loc, v) ->
            Some
              (fun (k : (a, _) continuation) ~(mem : Memory.t) ->
                Logger.mem mem (`Update_loc (loc, v));
                continue k () ~mem:(Memory.update mem ~loc ~obj:v))
        | _ -> None);
  }

let treemem_h =
  {
    retc =
      (fun v ~treemem ->
        Logger.treemem treemem `Ret;
        (v, treemem));
    exnc = raise;
    effc =
      (fun (type a) (eff : a t) ->
        match eff with
        (* in eval *)
        | Lookup_st (path, label) ->
            Some
              (fun (k : (a, _) continuation) ~(treemem : Tree_mem.t) ->
                Logger.treemem treemem (`Lookup_st (path, label));
                continue k (Tree_mem.lookup_st treemem ~path ~label) ~treemem)
        | Update_st (path, label, (v, q)) ->
            Some
              (fun (k : (a, _) continuation) ~(treemem : Tree_mem.t) ->
                Logger.treemem treemem (`Update_st (path, label, (v, q)));
                continue k ()
                  ~treemem:(Tree_mem.update_st treemem ~path ~label (v, q)))
        | Get_dec path ->
            Some
              (fun (k : (a, _) continuation) ~(treemem : Tree_mem.t) ->
                Logger.treemem treemem (`Get_dec path);
                continue k (Tree_mem.get_dec treemem ~path) ~treemem)
        | Set_dec (path, dec) ->
            Some
              (fun (k : (a, _) continuation) ~(treemem : Tree_mem.t) ->
                Logger.treemem treemem (`Set_dec (path, dec));
                continue k () ~treemem:(Tree_mem.set_dec treemem ~path dec))
        | Enq_eff (path, clos) ->
            Some
              (fun (k : (a, _) continuation) ~(treemem : Tree_mem.t) ->
                Logger.treemem treemem (`Enq_eff (path, clos));
                continue k () ~treemem:(Tree_mem.enq_eff treemem ~path clos))
        (* in render *)
        | Alloc_pt ->
            Some
              (fun (k : (a, _) continuation) ~(treemem : Tree_mem.t) ->
                Logger.treemem treemem `Alloc_pt;
                continue k (Tree_mem.alloc_pt treemem) ~treemem)
        | Lookup_ent path ->
            Some
              (fun (k : (a, _) continuation) ~(treemem : Tree_mem.t) ->
                Logger.treemem treemem (`Lookup_ent path);
                continue k (Tree_mem.lookup_ent treemem ~path) ~treemem)
        | Update_ent (path, ent) ->
            Some
              (fun (k : (a, _) continuation) ~(treemem : Tree_mem.t) ->
                Logger.treemem treemem (`Update_ent (path, ent));
                continue k () ~treemem:(Tree_mem.update_ent treemem ~path ent))
        | _ -> None);
  }

let value_exn exn v =
  Option.value_exn v ~error:(Error.of_exn exn ~backtrace:`Get)

let int_of_value_exn v = v |> Value.to_int |> value_exn Type_error
let bool_of_value_exn v = v |> Value.to_bool |> value_exn Type_error
let loc_of_value_exn v = v |> Value.to_loc |> value_exn Type_error
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
      let p = eval pred |> bool_of_value_exn in
      if p then eval con else eval alt
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
            if Path.(path = self_pt) && Phase.(phase <> P_effect) then Retry
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
      | Uplus, Int i -> Int i
      | Uminus, Int i -> Int ~-i
      | _, _ -> raise Type_error)
  | Bop { op; left; right } -> (
      let v1 = eval left in
      let v2 = eval right in
      match (op, v1, v2) with
      | Eq, Unit, Unit -> Bool true
      | Eq, Bool b1, Bool b2 -> Bool Bool.(b1 = b2)
      | Eq, Int i1, Int i2 -> Bool (i1 = i2)
      | Lt, Int i1, Int i2 -> Bool (i1 < i2)
      | Gt, Int i1, Int i2 -> Bool (i1 > i2)
      | Le, Int i1, Int i2 -> Bool (i1 <= i2)
      | Ge, Int i1, Int i2 -> Bool (i1 >= i2)
      | Ne, Unit, Unit -> Bool false
      | Ne, Bool b1, Bool b2 -> Bool Bool.(b1 <> b2)
      | Ne, Int i1, Int i2 -> Bool (i1 <> i2)
      | And, Bool b1, Bool b2 -> Bool (b1 && b2)
      | Or, Bool b1, Bool b2 -> Bool (b1 || b2)
      | Plus, Int i1, Int i2 -> Int (i1 + i2)
      | Minus, Int i1, Int i2 -> Int (i1 - i2)
      | Times, Int i1, Int i2 -> Int (i1 * i2)
      | _, _, _ -> raise Type_error)
  | Alloc ->
      let loc = perform (Alloc_loc Obj.empty) in
      Loc loc
  | Get { obj; field } ->
      let l = eval obj |> loc_of_value_exn in
      perform (Lookup_loc l) |> Obj.lookup ~field
  | Set { obj; field; value } ->
      let loc = eval obj |> loc_of_value_exn in
      let v = eval value in
      let old_obj = perform (Lookup_loc loc) in
      let new_obj = Obj.update old_obj ~field ~value:v in
      perform (Update_loc (loc, new_obj));
      Unit
  | GetIdx { obj; idx } ->
      let loc = eval obj |> loc_of_value_exn in
      let i = eval idx |> int_of_value_exn in
      let obj = perform (Lookup_loc loc) in
      Obj.lookup obj ~field:(Int.to_string i)
  | SetIdx { obj; idx; value } ->
      let loc = eval obj |> loc_of_value_exn in
      let i = eval idx |> int_of_value_exn in
      let old_obj = perform (Lookup_loc loc) in
      let value = eval value in
      let new_obj = Obj.update old_obj ~field:(Int.to_string i) ~value in
      perform (Update_loc (loc, new_obj));
      Unit

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
      match_with
        (eval_mult ~re_render:(re_render + 1))
        expr ptph_h ~ptph:(path, P_retry)
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
        ( (eval_mult |> fun f x -> match_with f x env_h ~env) |> fun f x ->
          match_with f x ptph_h ~ptph:(path, P_init) )
          body
        |> vss_of_value_exn
      in
      render path vss;

      Path path

let rec update (path : Path.t) (arg : value option) : bool =
  Logger.update path;
  let { part_view; children } = perform (Lookup_ent path) in
  match part_view with
  | Root -> update_idle children
  | Node { comp_spec = { comp = { param; body; _ }; env; arg = arg' }; dec; _ }
    -> (
      match (dec, arg) with
      | Retry, _ -> assert false
      | Idle, None -> update_idle children
      | Idle, Some _ | Update, _ ->
          (* Invariant: if arg is Some _, then it is different from arg' *)
          perform (Set_dec (path, Idle));
          let env =
            Env.extend env ~id:param ~value:(Option.value arg ~default:arg')
          in
          let vss =
            ( (eval_mult |> fun f x -> match_with f x env_h ~env) |> fun f x ->
              match_with f x ptph_h ~ptph:(path, P_update) )
              body
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
          let updated = reconcile path old_trees vss in
          let dec = perform (Get_dec path) in
          updated || Decision.(dec <> Idle))

and update_idle (children : tree Snoc_list.t) : bool =
  Snoc_list.fold children ~init:false ~f:(fun acc t -> acc || update1 t None)

and update1 (t : tree) (arg : value option) : bool =
  Logger.update1 t;
  match t with Leaf_null | Leaf_int _ -> false | Path path -> update path arg

and reconcile (path : Path.t) (old_trees : tree option list)
    (vss : view_spec list) : bool =
  Logger.reconcile path old_trees vss;
  List.fold2_exn old_trees vss ~init:false ~f:(fun acc old_tree vs ->
      let updated, t = reconcile1 old_tree vs in
      let ({ children; _ } as ent) = perform (Lookup_ent path) in
      perform
        (Update_ent (path, { ent with children = Snoc_list.(children ||> t) }));
      acc || updated)

and reconcile1 (old_tree : tree option) (vs : view_spec) : bool * tree =
  Logger.reconcile1 old_tree vs;
  match (old_tree, vs) with
  | Some (Leaf_null as t), Vs_null -> (false, t)
  | Some (Leaf_int i as t), Vs_int j when i = j -> (false, t)
  | Some (Path pt as t), (Vs_comp { comp = { name; _ }; arg; _ } as vs) -> (
      let { part_view; _ } = perform (Lookup_ent pt) in
      match part_view with
      | Root -> assert false
      | Node { comp_spec = { comp = { name = name'; _ }; arg = arg'; _ }; _ } ->
          if Id.(name = name') then
            (update1 t (if Value.(arg = arg') then None else Some arg), t)
          else (true, render1 vs))
  | _, vs -> (true, render1 vs)

let rec commit_effs (path : Path.t) : unit =
  Logger.commit_effs path;
  let { part_view; children } = perform (Lookup_ent path) in
  (match part_view with
  | Root -> ()
  | Node { eff_q; _ } -> (
      Job_q.iter eff_q ~f:(fun { body; env; _ } ->
          ( (eval |> fun f x -> match_with f x env_h ~env) |> fun f x ->
            match_with f x ptph_h ~ptph:(path, P_effect) )
            body
          |> ignore);

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
  let vss = match_with eval_top prog env_h ~env:Env.empty in
  let path = perform Alloc_pt in
  perform (Update_ent (path, { part_view = Root; children = [] }));
  render path vss;
  commit_effs path;
  path

let step_path (path : Path.t) : bool =
  Logger.step_path path;
  let has_updates = update path None in
  if has_updates then commit_effs path;
  has_updates

type run_info = { steps : int; mem : Memory.t; treemem : Tree_mem.t }

let run ?(fuel : int option) (prog : Prog.t) : run_info =
  Logger.run prog;
  let driver () =
    let cnt = ref 1 in
    Logs.info (fun m -> m "Step prog %d" !cnt);
    let path = step_prog prog in
    let rec loop () =
      Logs.info (fun m -> m "Step path %d" (!cnt + 1));
      if step_path path then (
        Int.incr cnt;
        match fuel with Some n when !cnt >= n -> () | _ -> loop ())
    in
    loop ();
    !cnt
  in
  let (steps, mem), treemem =
    ( (driver |> fun f x -> match_with f x mem_h ~mem:Memory.empty) |> fun f x ->
      match_with f x treemem_h ~treemem:Tree_mem.empty )
      ()
  in
  { steps; mem; treemem }
