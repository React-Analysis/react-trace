open! Base
open Stdlib.Effect
open Syntax
open Domains

type _ eff += Rd_env : env eff | In_env : env -> (('b -> 'a) -> 'b -> 'a) eff

type _ eff +=
  | Lookup_st : Label.t -> (value * job_q) eff
  | Update_st : (Label.t * (value * job_q)) -> unit eff

type _ eff +=
  | Part_view_set_dec : path * Label.t * decision -> unit eff
  | Part_view_enq_set : path * Label.t * clos -> unit eff

type _ eff += Set_dec : decision -> unit eff
type _ eff += Rd_phase : phase eff
type _ eff += Rd_path : path eff
type _ eff += Enq_eff : clos -> unit eff

exception Unbound_var of string
exception Type_error
exception Invalid_phase

let rec eval : type a. a Expr.t -> value = function
  | Const (Unit ()) -> Unit
  | Const (Int i) -> Int i
  | Var x -> (
      let env = perform Rd_env in
      match Map.find env x with Some v -> v | None -> raise (Unbound_var x))
  | View es ->
      let vss =
        List.fold es ~init:[] ~f:(fun vss e ->
            let vs =
              match eval e with
              | Unit -> Vs_null
              | Int i -> Vs_int i
              | Comp_thunk t -> Vs_comp t
              | _ -> raise Type_error
            in
            vs :: vss)
      in
      View_spec vss
  | Cond { pred; con; alt } -> (
      match eval pred with
      | Int 0 -> eval alt
      | Int _ -> eval con
      | _ -> raise Type_error)
  | Fn { param; body } -> Clos { param; body; env = perform Rd_env }
  | App { fn; arg } -> (
      match eval fn with
      | Clos { param; body; env } ->
          let env = Env.extend env ~id:param ~value:(eval arg) in
          perform (In_env env) eval body
      | Comp_clos { comp; env } -> Comp_thunk { comp; env; arg = eval arg }
      | Set_clos { label; path } ->
          let clos =
            (* Argument to the setter should be a setting thunk *)
            match eval arg with Clos c -> c | _ -> raise Type_error
          in

          let self_path = perform Rd_path in
          if Tree_path.(path = self_path) then (
            (* Set self's state *)
            perform
              (Set_dec
                 (match perform Rd_phase with P_effect -> Update | _ -> Retry));

            let v, q = perform (Lookup_st label) in
            perform (Update_st (label, (v, Job_q.enqueue q clos)));
            Unit)
          else (
            (* Set other's state *)
            perform (Part_view_set_dec (path, label, Update));
            perform (Part_view_enq_set (path, label, clos));
            Unit)
      | _ -> raise Type_error)
  | Let { id; bound; body } ->
      let value = eval bound in
      let env = Env.extend (perform Rd_env) ~id ~value in
      perform (In_env env) eval body
  | Stt { label; stt; set; init; body } -> (
      let path = perform Rd_path in
      match perform Rd_phase with
      | P_init ->
          let v = eval init in
          perform (Update_st (label, (v, empty_setq)));
          let env =
            perform Rd_env
            |> Env.extend ~id:stt ~value:v
            |> Env.extend ~id:set ~value:(Set_clos { label; path })
          in
          perform (In_env env) eval body
      | P_update | P_retry ->
          let v_prev, q = perform (Lookup_st label) in
          (* Run the setting thunks in the set queue *)
          let v_curr =
            Job_q.fold q ~init:v_prev ~f:(fun value { param; body; env } ->
                let env = Env.extend env ~id:param ~value in
                perform (In_env env) eval body)
          in
          if v_prev <> v_curr then perform (Set_dec Update);
          perform (Update_st (label, (v_curr, Job_q.empty)));
          let env =
            perform Rd_env
            |> Env.extend ~id:stt ~value:v_curr
            |> Env.extend ~id:set ~value:(Set_clos { label; path })
          in
          perform (In_env env) eval body
      | P_effect -> raise Invalid_phase)
  | Eff e -> (
      match perform Rd_phase with
      | P_effect -> raise Invalid_phase
      | _ ->
          perform (Enq_eff { param = Id.unit; body = e; env = perform Rd_env });
          Unit)
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
