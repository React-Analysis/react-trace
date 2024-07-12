open! Base
open Syntax
open Domains

let ptph ((pt, ph) : Path.t * Phase.t) = function
  | `Ret ->
      Logs.debug (fun m ->
          m "ptph_h Ret [pt: %a, ph: %a]" Sexp.pp_hum (Path.sexp_of_t pt)
            Sexp.pp_hum (Phase.sexp_of_t ph))
  | `Rd_pt ->
      Logs.debug (fun m ->
          m "ptph_h Rd_pt [pt: %a, ph: %a]" Sexp.pp_hum (Path.sexp_of_t pt)
            Sexp.pp_hum (Phase.sexp_of_t ph))
  | `Rd_ph ->
      Logs.debug (fun m ->
          m "ptph_h Rd_ph [pt: %a, ph: %a]" Sexp.pp_hum (Path.sexp_of_t pt)
            Sexp.pp_hum (Phase.sexp_of_t ph))

let env env = function
  | `Ret ->
      Logs.debug (fun m ->
          m "env_h Ret [env: %a]" Sexp.pp_hum (Env.sexp_of_t env))
  | `Rd_env ->
      Logs.debug (fun m ->
          m "env_h Rd_env [env: %a]" Sexp.pp_hum (Env.sexp_of_t env))
  | `In_env env' ->
      Logs.debug (fun m ->
          m "env_h In_env [env: %a, env': %a]" Sexp.pp_hum (Env.sexp_of_t env)
            Sexp.pp_hum (Env.sexp_of_t env'))

let mem mem = function
  | `Ret ->
      Logs.debug (fun m ->
          m "mem_h Ret [mem: %a]" Sexp.pp_hum (Tree_mem.sexp_of_t mem))
  | `Lookup_st (path, label) ->
      Logs.debug (fun m ->
          m "mem_h Lookup_st [mem: %a, path: %a, label: %a]" Sexp.pp_hum
            (Tree_mem.sexp_of_t mem) Sexp.pp_hum (Path.sexp_of_t path)
            Sexp.pp_hum (Label.sexp_of_t label))
  | `Update_st (path, label, (v, q)) ->
      Logs.debug (fun m ->
          m "mem_h Update_st [mem: %a, path: %a, label: %a, v: %a, q: %a]"
            Sexp.pp_hum (Tree_mem.sexp_of_t mem) Sexp.pp_hum
            (Path.sexp_of_t path) Sexp.pp_hum (Label.sexp_of_t label)
            Sexp.pp_hum (Value.sexp_of_t v) Sexp.pp_hum (Job_q.sexp_of_t q))
  | `Get_dec path ->
      Logs.debug (fun m ->
          m "mem_h Get_dec [mem: %a, path: %a]" Sexp.pp_hum
            (Tree_mem.sexp_of_t mem) Sexp.pp_hum (Path.sexp_of_t path))
  | `Set_dec (path, dec) ->
      Logs.debug (fun m ->
          m "mem_h Set_dec [mem: %a, path: %a, dec: %a]" Sexp.pp_hum
            (Tree_mem.sexp_of_t mem) Sexp.pp_hum (Path.sexp_of_t path)
            Sexp.pp_hum (Decision.sexp_of_t dec))
  | `Enq_eff (path, clos) ->
      Logs.debug (fun m ->
          m "mem_h Enq_eff [mem: %a, path: %a, clos: %a]" Sexp.pp_hum
            (Tree_mem.sexp_of_t mem) Sexp.pp_hum (Path.sexp_of_t path)
            Sexp.pp_hum (Value.sexp_of_clos clos))
  | `Alloc_pt ->
      Logs.debug (fun m ->
          m "mem_h Alloc_pt [mem: %a]" Sexp.pp_hum (Tree_mem.sexp_of_t mem))
  | `Lookup_ent path ->
      Logs.debug (fun m ->
          m "mem_h Lookup_ent [mem: %a, path: %a]" Sexp.pp_hum
            (Tree_mem.sexp_of_t mem) Sexp.pp_hum (Path.sexp_of_t path))
  | `Update_ent (path, ent) ->
      Logs.debug (fun m ->
          m "mem_h Update_ent [mem: %a, path: %a, ent: %a]" Sexp.pp_hum
            (Tree_mem.sexp_of_t mem) Sexp.pp_hum (Path.sexp_of_t path)
            Sexp.pp_hum (Value.sexp_of_entry ent))

let eval expr =
  Logs.debug (fun m -> m "eval %a" Sexp.pp_hum (Expr.sexp_of_t expr))

let eval_mult expr =
  Logs.debug (fun m -> m "eval_mult %a" Sexp.pp_hum (Expr.sexp_of_t expr))

let render path vss =
  Logs.debug (fun m ->
      m "render [path: %a, vss: %a]" Sexp.pp (Path.sexp_of_t path) Sexp.pp
        (List.sexp_of_t Value.sexp_of_view_spec vss))

let render1 vs =
  Logs.debug (fun m ->
      m "render1 [vs: %a]" Sexp.pp (Value.sexp_of_view_spec vs))

let update path =
  Logs.debug (fun m -> m "update [path: %a]" Sexp.pp (Path.sexp_of_t path))

let update1 t =
  Logs.debug (fun m -> m "update1 [t: %a]" Sexp.pp (Value.sexp_of_tree t))

let reconcile path old_trees vss =
  Logs.debug (fun m ->
      m "reconcile [path: %a, old_trees: %a, vss: %a]" Sexp.pp
        (Path.sexp_of_t path) Sexp.pp
        (List.sexp_of_t (Option.sexp_of_t Value.sexp_of_tree) old_trees)
        Sexp.pp
        (List.sexp_of_t Value.sexp_of_view_spec vss))

let reconcile1 old_tree vs =
  Logs.debug (fun m ->
      m "reconcile1 [old_tree: %a, vs: %a]" Sexp.pp
        ((Option.sexp_of_t Value.sexp_of_tree) old_tree)
        Sexp.pp
        (Value.sexp_of_view_spec vs))

let commit_effs path =
  Logs.debug (fun m -> m "commit_effs [path: %a]" Sexp.pp (Path.sexp_of_t path))

let commit_effs1 t =
  Logs.debug (fun m -> m "commit_effs1 [t: %a]" Sexp.pp (Value.sexp_of_tree t))

let eval_top prog =
  Logs.debug (fun m -> m "eval_top %a" Sexp.pp_hum (Prog.sexp_of_t prog))

let step_prog prog =
  Logs.debug (fun m -> m "step_prog %a" Sexp.pp_hum (Prog.sexp_of_t prog))

let step_path path =
  Logs.debug (fun m -> m "step_path %a" Sexp.pp_hum (Path.sexp_of_t path))

let run prog =
  Logs.debug (fun m -> m "run %a" Sexp.pp_hum (Prog.sexp_of_t prog))
