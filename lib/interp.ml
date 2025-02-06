open! Base
open Stdlib.Effect
open Stdlib.Effect.Deep
open Lib_domains
open Syntax
open Concrete_domains
open Interp_effects

let re_render_limit_h (type a b) (f : a -> b) (x : a) : re_render_limit:int -> b
    =
  match f x with
  | v -> fun ~re_render_limit:_ -> v
  | effect Re_render_limit, k ->
      fun ~re_render_limit -> continue k re_render_limit ~re_render_limit

let ptph_h (type a b) (f : a -> b) (x : a) : ptph:Path.t * phase -> b =
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

let rec env_h : type a b. (a -> b) -> a -> env:Env.t -> b =
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

let mem_h (type a b) (f : a -> b) (x : a) : mem:Memory.t -> b * Memory.t =
  match f x with
  | v ->
      fun ~mem ->
        Logger.mem mem `Ret;
        (v, mem)
  | effect Alloc_addr obj, k ->
      fun ~mem ->
        Logger.mem mem `Alloc_addr;
        let addr = Memory.alloc mem in
        let mem = Memory.update mem ~addr ~obj in
        continue k addr ~mem
  | effect Lookup_addr addr, k ->
      fun ~mem ->
        Logger.mem mem (`Lookup_addr addr);
        continue k (Memory.lookup mem ~addr) ~mem
  | effect Update_addr (addr, v), k ->
      fun ~mem ->
        Logger.mem mem (`Update_addr (addr, v));
        continue k () ~mem:(Memory.update mem ~addr ~obj:v)

let treemem_h (type a b) (f : a -> b) (x : a) :
    treemem:Tree_mem.t -> b * Tree_mem.t =
  match f x with
  | v ->
      fun ~treemem ->
        Logger.treemem treemem `Ret;
        (v, treemem)
  | effect Lookup_st (path, label), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Lookup_st (path, label));
        continue k (Tree_mem.lookup_st treemem ~path ~label) ~treemem
  | effect Update_st (path, label, (v, q)), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Update_st (path, label, (v, q)));
        continue k () ~treemem:(Tree_mem.update_st treemem ~path ~label (v, q))
  | effect Get_dec path, k ->
      fun ~treemem ->
        Logger.treemem treemem (`Get_dec path);
        continue k (Tree_mem.get_dec treemem ~path) ~treemem
  | effect Set_dec (path, dec), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Set_dec (path, dec));
        continue k () ~treemem:(Tree_mem.set_dec treemem ~path dec)
  | effect Set_arg (path, arg), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Set_arg (path, arg));
        continue k () ~treemem:(Tree_mem.set_arg treemem ~path arg)
  | effect Enq_eff (path, clos), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Enq_eff (path, clos));
        continue k () ~treemem:(Tree_mem.enq_eff treemem ~path clos)
  (* NOTE: in render *)
  | effect Alloc_pt, k ->
      fun ~treemem ->
        Logger.treemem treemem `Alloc_pt;
        continue k (Tree_mem.alloc_pt treemem) ~treemem
  | effect Lookup_ent path, k ->
      fun ~treemem ->
        Logger.treemem treemem (`Lookup_ent path);
        continue k (Tree_mem.lookup_ent treemem ~path) ~treemem
  | effect Update_ent (path, ent), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Update_ent (path, ent));
        continue k () ~treemem:(Tree_mem.update_ent treemem ~path ent)
  (* NOTE: instrumentation *)
  | effect Get_root_pt, k ->
      fun ~treemem -> continue k (Tree_mem.root_pt treemem) ~treemem

let value_exn exn v =
  Option.value_exn v ~error:(Error.of_exn exn ~backtrace:`Get)

let int_of_value_exn v = v |> Value.to_int |> value_exn Type_error
let bool_of_value_exn v = v |> Value.to_bool |> value_exn Type_error
let string_of_value_exn v = v |> Value.to_string |> value_exn Type_error
let addr_of_value_exn v = v |> Value.to_addr |> value_exn Type_error
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
  match expr.desc with
  | Const Unit -> Unit
  | Const (Bool b) -> Bool b
  | Const (Int i) -> Int i
  | Const (String s) -> String s
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
      let addr = perform (Alloc_addr Obj.empty) in
      Addr addr
  | Get { obj; idx } ->
      let addr = eval obj |> addr_of_value_exn in
      let i = eval idx |> string_of_value_exn in
      let obj = perform (Lookup_addr addr) in
      Obj.lookup obj ~field:i
  | Set { obj; idx; value } ->
      let addr = eval obj |> addr_of_value_exn in
      let i = eval idx |> string_of_value_exn in
      let old_obj = perform (Lookup_addr addr) in
      let value = eval value in
      let new_obj = Obj.update old_obj ~field:i ~value in
      perform (Update_addr (addr, new_obj));
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
      let re_render = re_render + 1 in
      perform
        (Checkpoint
           { msg = "Will retry"; checkpoint = Retry_start (re_render, path) });
      ptph_h ~ptph:(path, P_retry) (eval_mult ~re_render) expr
  | Idle | Update -> v

let alloc_tree (vs : view_spec) : tree =
  Logger.alloc_tree vs;
  match vs with
  | Vs_null -> Leaf_null
  | Vs_int i -> Leaf_int i
  | Vs_comp comp_spec ->
      let pt = perform Alloc_pt in
      let part_view =
        Node
          {
            comp_spec;
            dec = Idle;
            st_store = St_store.empty;
            eff_q = Job_q.empty;
          }
      in
      perform (Update_ent (pt, { part_view; children = [] }));
      Path pt

let mount_tree (path : Path.t) ?(idx : int option) (tree : tree) : unit =
  Logger.mount_tree path ?idx tree;
  let ({ children; _ } as ent) = perform (Lookup_ent path) in
  let children =
    let open Snoc_list in
    match idx with
    | None -> children ||> tree
    | Some i -> replace children i tree
  in
  perform (Update_ent (path, { ent with children }))

let rec render (path : Path.t) (vss : view_spec list) : unit =
  Logger.render path vss;
  perform (Checkpoint { msg = "Render"; checkpoint = Render_check path });
  List.iter vss ~f:(alloc_child_and_render1 path);
  perform (Checkpoint { msg = "Rendered"; checkpoint = Render_finish path })

and render1 (vs : view_spec) (t : tree) : unit =
  Logger.render1 vs;
  match (vs, t) with
  | Vs_null, Leaf_null -> ()
  | Vs_int i, Leaf_int i' when i = i' -> ()
  | Vs_comp { comp = { param; body; _ }; env; arg }, Path path ->
      let env = Env.extend env ~id:param ~value:arg in
      let vss =
        (eval_mult |> env_h ~env |> ptph_h ~ptph:(path, P_init)) body
        |> vss_of_value_exn
      in
      render path vss
  | _, _ -> assert false

and alloc_child_and_render1 (path : Path.t) ?(idx : int option) (vs : view_spec)
    : unit =
  let t = alloc_tree vs in
  mount_tree path ?idx t;
  render1 vs t

let rec update (path : Path.t) (arg : value option) : bool =
  Logger.update path;
  perform
    (Checkpoint { msg = "Render (update)"; checkpoint = Render_check path });
  let { part_view; children } = perform (Lookup_ent path) in
  let updated =
    match part_view with
    | Root -> update_idle children
    | Node
        { comp_spec = { comp = { param; body; _ }; env; arg = arg' }; dec; _ }
      -> (
        match (dec, arg) with
        | Retry, _ -> assert false
        | Idle, None -> update_idle children
        (* NOTE: Invariant: if arg is Some _, then it is different from arg' *)
        | Idle, Some _ | Update, _ ->
            perform (Set_dec (path, Idle));
            let arg = Option.value arg ~default:arg' in
            perform (Set_arg (path, arg));
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
               younger sibling are. That's why we are resetting the children
               list and then snoc each child again in the reconcile function. We
               should verify this behavior. *)
            (* NOTE: We don't do this any more, since we are modelling as an
               'in-place' update now *)
            (*let ent = perform (Lookup_ent path) in*)
            (*perform (Update_ent (path, { ent with children = [] }));*)
            let updated = reconcile path old_trees vss in
            let dec = perform (Get_dec path) in
            updated || Decision.(dec <> Idle))
  in
  if updated then
    perform
      (Checkpoint { msg = "Rendered (update)"; checkpoint = Render_finish path });
  updated

and update_idle (children : tree Snoc_list.t) : bool =
  Snoc_list.fold children ~init:false ~f:(fun acc t -> acc || update1 t None)

and update1 (t : tree) (arg : value option) : bool =
  Logger.update1 t;
  match t with Leaf_null | Leaf_int _ -> false | Path path -> update path arg

and reconcile (path : Path.t) (old_trees : tree option list)
    (vss : view_spec list) : bool =
  Logger.reconcile path old_trees vss;
  Util.fold2i_exn old_trees vss ~init:false ~f:(fun idx acc old_tree vs ->
      acc || reconcile1 path idx old_tree vs)

and reconcile1 (path : Path.t) (idx : int) (old_tree : tree option)
    (vs : view_spec) : bool =
  Logger.reconcile1 old_tree vs;
  match (old_tree, vs) with
  | Some Leaf_null, Vs_null -> false
  | Some (Leaf_int i), Vs_int j when i = j -> false
  | Some (Path pt as t), (Vs_comp { comp = { name; _ }; arg; _ } as vs) -> (
      let { part_view; _ } = perform (Lookup_ent pt) in
      match part_view with
      | Root -> assert false
      | Node { comp_spec = { comp = { name = name'; _ }; arg = arg'; _ }; _ } ->
          if Id.(name = name') then
            update1 t (if Value.(arg = arg') then None else Some arg)
          else (
            alloc_child_and_render1 path ~idx vs;
            true))
  | _, vs ->
      alloc_child_and_render1 path ~idx vs;
      true

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
  Snoc_list.iter children ~f:commit_effs1;
  perform
    (Checkpoint { msg = "After effects"; checkpoint = Effects_finish path })

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
  let vss = env_h ~env:Env.empty eval_top prog in
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

type 'recording run_info = {
  steps : int;
  mem : Memory.t;
  treemem : Tree_mem.t;
  recording : 'recording;
}

let run (type recording) ?(fuel : int option)
    ~(recorder : (module Recorder_intf.Intf with type recording = recording))
    (prog : Prog.t) : recording run_info =
  Logger.run prog;

  let driver () =
    let cnt = ref 1 in
    Logs.info (fun m -> m "Step prog %d" !cnt);
    let root_path = step_prog prog in

    let rec loop () =
      Logs.info (fun m -> m "Step path %d" (!cnt + 1));
      if step_path root_path then (
        Int.incr cnt;
        match fuel with Some n when !cnt >= n -> () | _ -> loop ())
    in
    loop ();
    !cnt
  in

  let driver () =
    let open (val recorder) in
    event_h ~recording:emp_recording driver ()
  in
  let driver () = treemem_h ~treemem:Tree_mem.empty driver () in
  let driver () = mem_h ~mem:Memory.empty driver () in
  let ((steps, recording), treemem), mem = driver () in
  { steps; mem; treemem; recording }
