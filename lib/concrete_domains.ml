open! Core
open Syntax

module M : Domains.S = struct
  module rec T : (Domains.T with type path = int) = struct
    type path = Path.t [@@deriving sexp_of]
    type env = Env.t [@@deriving sexp_of]
    type addr = Addr.t [@@deriving sexp_of]
    type obj = Obj.t [@@deriving sexp_of]
    type st_store = St_store.t [@@deriving sexp_of]
    type job_q = Job_q.t [@@deriving sexp_of]

    type clos = { param : Id.t; body : Expr.hook_free_t; env : env }
    [@@deriving sexp_of]

    type value =
      | Unit
      | Bool of bool
      | Int of int
      | String of string
      | Addr of addr
      | View_specs of view_spec list
      | Clos of clos
      | Set_clos of set_clos
      | Comp_clos of comp_clos
      | Comp_spec of comp_spec

    and set_clos = { label : Label.t; path : path }
    and comp_clos = { comp : Prog.comp; env : env }
    and comp_spec = { comp : Prog.comp; env : env; arg : value }

    and view_spec = Vs_null | Vs_int of int | Vs_comp of comp_spec
    [@@deriving sexp_of]

    type phase = P_init | P_update | P_retry | P_effect [@@deriving sexp_of]
    type decision = Idle | Retry | Update [@@deriving sexp_of]

    type part_view =
      | Root
      | Node of {
          comp_spec : comp_spec;
          dec : decision;
          st_store : st_store;
          eff_q : job_q;
        }
    [@@deriving sexp_of]

    type tree = Leaf_null | Leaf_int of int | Path of path
    [@@deriving sexp_of]

    type entry = { part_view : part_view; children : tree Snoc_list.t }
    [@@deriving sexp_of]
  end

  and Path : (Domains.Path with type t = T.path) = Int

  and Env : (Domains.Env with type value = T.value and type t = T.env) = struct
    type value = T.value [@@deriving sexp_of]
    type t = value Id.Map.t [@@deriving sexp_of]

    let empty = Id.Map.empty
    let lookup env ~id = Map.find env id
    let extend env ~id ~value = Map.set env ~key:id ~data:value
  end

  and Addr : (Domains.Addr with type t = T.addr) = Int

  and Obj : (Domains.Obj with type value = T.value and type t = T.obj) = struct
    type value = T.value [@@deriving sexp_of]
    type t = value Id.Map.t [@@deriving sexp_of]

    let empty = Id.Map.empty
    let lookup obj ~field = Map.find obj field |> Option.value ~default:T.Unit
    let update obj ~field ~value = Map.set obj ~key:field ~data:value
  end

  and Memory : (Domains.Memory with type obj = T.obj and type addr = T.addr) =
  struct
    type obj = T.obj [@@deriving sexp_of]
    type addr = T.addr [@@deriving sexp_of]
    type t = obj Map.M(Addr).t [@@deriving sexp_of]

    let empty = Map.empty (module Addr)
    let alloc = Map.length
    let lookup memory ~addr = Map.find_exn memory addr
    let update memory ~addr ~obj = Map.set memory ~key:addr ~data:obj
  end

  and St_store :
    (Domains.St_store
      with type value = T.value
       and type job_q = T.job_q
       and type t = T.st_store) = struct
    type value = T.value [@@deriving sexp_of]
    type job_q = Job_q.t [@@deriving sexp_of]
    type t = (value * job_q) Label.Map.t [@@deriving sexp_of]

    let empty = Label.Map.empty
    let lookup store ~label = Map.find_exn store label
    let update store ~label ~value = Map.set store ~key:label ~data:value
    let to_alist store = Map.to_alist ~key_order:`Increasing store
  end

  and Job_q : (Domains.Job_q with type elt := T.clos and type t = T.job_q) =
  Batched_queue.M (struct
    type t = T.clos [@@deriving sexp_of]
  end)

  module Tree_mem :
    Domains.Tree_mem
      with type value = T.value
       and type path = T.path
       and type job_q = T.job_q
       and type decision = T.decision
       and type clos = T.clos
       and type entry = T.entry = struct
    type value = T.value [@@deriving sexp_of]
    type path = Path.t [@@deriving sexp_of]
    type job_q = Job_q.t [@@deriving sexp_of]
    type decision = T.decision [@@deriving sexp_of]
    type clos = T.clos [@@deriving sexp_of]
    type entry = T.entry [@@deriving sexp_of]
    type t = entry Map.M(Path).t [@@deriving sexp_of]

    let empty = Map.empty (module Path)

    open T

    let lookup_st (tree_mem : t) ~(path : path) ~(label : Label.t) :
        value * job_q =
      let { part_view; _ } = Map.find_exn tree_mem path in
      match part_view with
      | Root -> assert false
      | Node { st_store; _ } -> St_store.lookup st_store ~label

    let update_st tree_mem ~path ~label (v, q) =
      let ({ part_view; _ } as entry) = Map.find_exn tree_mem path in
      match part_view with
      | Root -> assert false
      | Node ({ st_store; _ } as n) ->
          let st_store = St_store.update st_store ~label ~value:(v, q) in
          Map.set tree_mem ~key:path
            ~data:{ entry with part_view = Node { n with st_store } }

    let get_dec tree_mem ~path =
      let { part_view; _ } = Map.find_exn tree_mem path in
      match part_view with Root -> assert false | Node { dec; _ } -> dec

    let set_dec tree_mem ~path dec =
      let ({ part_view; _ } as entry) = Map.find_exn tree_mem path in
      match part_view with
      | Root -> assert false
      | Node n ->
          Map.set tree_mem ~key:path
            ~data:{ entry with part_view = Node { n with dec } }

    let enq_eff tree_mem ~path clos =
      let ({ part_view; _ } as entry) = Map.find_exn tree_mem path in
      match part_view with
      | Root -> assert false
      | Node ({ eff_q; _ } as n) ->
          let eff_q = Job_q.enqueue eff_q clos in
          Map.set tree_mem ~key:path
            ~data:{ entry with part_view = Node { n with eff_q } }

    let alloc_pt = Map.length
    let lookup_ent tree_mem ~path = Map.find_exn tree_mem path

    let update_ent tree_mem ~path ent =
      Logs.debug (fun m -> m "update_ent: %a" Sexp.pp_hum (Path.sexp_of_t path));
      Map.set tree_mem ~key:path ~data:ent
  end

  include T

  module Value = struct
    type nonrec view_spec = view_spec
    type nonrec clos = clos
    type nonrec addr = addr
    type t = value

    let to_bool = function Bool b -> Some b | _ -> None
    let to_int = function Int i -> Some i | _ -> None
    let to_string = function String s -> Some s | _ -> None
    let to_addr = function Addr l -> Some l | _ -> None

    let to_vs = function
      | Unit -> Some Vs_null
      | Int i -> Some (Vs_int i)
      | Comp_spec t -> Some (Vs_comp t)
      | _ -> None

    let to_vss = function View_specs vss -> Some vss | _ -> None
    let to_clos = function Clos c -> Some c | _ -> None

    let equal v1 v2 =
      match (v1, v2) with
      | Unit, Unit -> true
      | Bool b1, Bool b2 -> Bool.(b1 = b2)
      | Int i1, Int i2 -> i1 = i2
      | Addr l1, Addr l2 -> Addr.(l1 = l2)
      | _, _ -> false

    let ( = ) = equal
    let ( <> ) v1 v2 = not (v1 = v2)
  end

  module Phase = struct
    type t = phase = P_init | P_update | P_retry | P_effect [@@deriving equal]

    let ( = ) = equal
    let ( <> ) p1 p2 = not (p1 = p2)
  end

  module Decision = struct
    type t = decision = Idle | Retry | Update [@@deriving equal]

    let ( = ) = equal
    let ( <> ) d1 d2 = not (d1 = d2)
  end
end

include M
