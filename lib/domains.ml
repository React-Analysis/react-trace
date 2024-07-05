open! Base
open Syntax
module Path = Util.Map_key (Int)

module rec T : sig
  type value =
    | Unit
    | Bool of bool
    | Int of int
    | View_spec of view_spec list
    | Clos of clos
    | Set_clos of set_clos
    | Comp_clos of comp_clos
    | Comp_spec of comp_spec

  and clos = { param : Id.t; body : hook_free Expr.t; env : Env.t }
  and set_clos = { label : Label.t; path : Path.t }
  and comp_clos = { comp : Prog.comp; env : Env.t }
  and comp_spec = { comp : Prog.comp; env : Env.t; arg : value }
  and view_spec = Vs_null | Vs_int of int | Vs_comp of comp_spec

  type phase = P_init | P_update | P_retry | P_effect
  type decision = Idle | Retry | Update

  type part_view =
    | Root
    | Node of {
        comp_spec : comp_spec;
        dec : decision;
        st_store : St_store.t;
        eff_q : Job_q.t;
      }

  type tree = Leaf_null | Leaf_int of int | Path of Path.t
  type entry = { part_view : part_view; children : tree Snoc_list.t }
end =
  T

and Value : sig
  type t = T.value

  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val to_bool : t -> bool option
  val to_int : t -> int option
  val to_vs : t -> T.view_spec option
  val to_vss : t -> T.view_spec list option
  val to_clos : t -> T.clos option
  val sexp_of_t : t -> Sexp.t
  val sexp_of_clos : T.clos -> Sexp.t
  val sexp_of_set_clos : T.set_clos -> Sexp.t
  val sexp_of_comp_clos : T.comp_clos -> Sexp.t
  val sexp_of_comp_spec : T.comp_spec -> Sexp.t
  val sexp_of_view_spec : T.view_spec -> Sexp.t
  val sexp_of_phase : T.phase -> Sexp.t
  val sexp_of_decision : T.decision -> Sexp.t
  val sexp_of_part_view : T.part_view -> Sexp.t
  val sexp_of_tree : T.tree -> Sexp.t
  val sexp_of_entry : T.entry -> Sexp.t
end = struct
  open T

  type t = value

  let equal v1 v2 =
    match (v1, v2) with
    | Unit, Unit -> true
    | Bool b1, Bool b2 -> Bool.(b1 = b2)
    | Int i1, Int i2 -> i1 = i2
    | _, _ -> false

  let ( = ) = equal
  let ( <> ) v1 v2 = not (v1 = v2)
  let to_bool = function Bool b -> Some b | _ -> None
  let to_int = function Int i -> Some i | _ -> None

  let to_vs = function
    | Unit -> Some Vs_null
    | Int i -> Some (Vs_int i)
    | Comp_spec t -> Some (Vs_comp t)
    | _ -> None

  let to_vss = function View_spec vss -> Some vss | _ -> None
  let to_clos = function Clos c -> Some c | _ -> None

  let rec sexp_of_t =
    let open Sexp_helper in
    function
    | Unit -> a "()"
    | Bool b -> Bool.sexp_of_t b
    | Int i -> Int.sexp_of_t i
    | Comp_spec s -> sexp_of_comp_spec s
    | View_spec vss -> l (List.map vss ~f:sexp_of_view_spec)
    | Clos clos -> sexp_of_clos clos
    | Set_clos set_clos -> sexp_of_set_clos set_clos
    | Comp_clos comp_clos -> sexp_of_comp_clos comp_clos

  and sexp_of_clos { param; body; env } =
    let open Sexp_helper in
    l [ a "Clos"; Id.sexp_of_t param; Expr.sexp_of_t body; Env.sexp_of_t env ]

  and sexp_of_set_clos { label; path } =
    let open Sexp_helper in
    l [ a "Set_clos"; Label.sexp_of_t label; Path.sexp_of_t path ]

  and sexp_of_comp_clos { comp; env } =
    let open Sexp_helper in
    l [ a "Comp_clos"; Prog.sexp_of_comp comp; Env.sexp_of_t env ]

  and sexp_of_comp_spec { comp; env; arg } =
    let open Sexp_helper in
    l
      [
        a "Comp_spec"; Prog.sexp_of_comp comp; Env.sexp_of_t env; sexp_of_t arg;
      ]

  and sexp_of_view_spec =
    let open Sexp_helper in
    function
    | Vs_null -> a "Vs_null"
    | Vs_int i -> l [ a "Vs_int"; Int.sexp_of_t i ]
    | Vs_comp comp_spec -> l [ a "Vs_comp"; sexp_of_comp_spec comp_spec ]

  and sexp_of_part_view =
    let open Sexp_helper in
    function
    | Root -> a "Root"
    | Node { comp_spec; dec; st_store; eff_q } ->
        l
          [
            a "Node";
            sexp_of_comp_spec comp_spec;
            sexp_of_decision dec;
            St_store.sexp_of_t st_store;
            Job_q.sexp_of_t eff_q;
          ]

  and sexp_of_phase =
    let open Sexp_helper in
    function
    | P_init -> a "P_init"
    | P_update -> a "P_update"
    | P_retry -> a "P_retry"
    | P_effect -> a "P_effect"

  and sexp_of_decision =
    let open Sexp_helper in
    function Idle -> a "Idle" | Retry -> a "Retry" | Update -> a "Update"

  and sexp_of_tree =
    let open Sexp_helper in
    function
    | Leaf_null -> a "Leaf_null"
    | Leaf_int i -> l [ a "Leaf_int"; Int.sexp_of_t i ]
    | Path p -> l [ a "Path"; Path.sexp_of_t p ]

  and sexp_of_entry { part_view; children } =
    let open Sexp_helper in
    l
      [
        l [ a "part_view"; sexp_of_part_view part_view ];
        l
          [
            a "children";
            l (List.map (Snoc_list.to_list children) ~f:sexp_of_tree);
          ];
      ]
end

and Env : sig
  type t

  val empty : t
  val lookup : t -> id:Id.t -> T.value option
  val extend : t -> id:Id.t -> value:T.value -> t
  val sexp_of_t : t -> Sexp.t
end = struct
  type t = T.value Id.Map.t

  let empty = Id.Map.empty
  let lookup env ~id = Map.find env id
  let extend env ~id ~value = Map.set env ~key:id ~data:value

  let sexp_of_t env =
    let open Sexp_helper in
    l
      (Map.to_alist env
      |> List.map ~f:(fun (id, value) ->
             l [ Id.sexp_of_t id; Value.sexp_of_t value ]))
end

and St_store : sig
  type t

  val empty : t
  val lookup : t -> label:Label.t -> T.value * Job_q.t
  val update : t -> label:Label.t -> value:T.value * Job_q.t -> t
  val sexp_of_t : t -> Sexp.t
end = struct
  type t = (T.value * Job_q.t) Label.Map.t

  let empty = Label.Map.empty
  let lookup store ~label = Map.find_exn store label
  let update store ~label ~value = Map.set store ~key:label ~data:value

  let sexp_of_t store =
    let open Sexp_helper in
    l
      (Map.to_alist store
      |> List.map ~f:(fun (label, (value, job_q)) ->
             l
               [
                 Label.sexp_of_t label;
                 l [ Value.sexp_of_t value; Job_q.sexp_of_t job_q ];
               ]))
end

and Job_q : (Batched_queue.S with type elt := T.clos) = Batched_queue.M (struct
  type t = T.clos

  let sexp_of_t = Value.sexp_of_clos
end)

include T

module Tree_mem : sig
  type t

  val empty : t
  val lookup_st : t -> path:Path.t -> label:Label.t -> value * Job_q.t
  val update_st : t -> path:Path.t -> label:Label.t -> value * Job_q.t -> t
  val get_dec : t -> path:Path.t -> decision
  val set_dec : t -> path:Path.t -> decision -> t
  val enq_eff : t -> path:Path.t -> clos -> t
  val alloc_pt : t -> Path.t
  val lookup_ent : t -> path:Path.t -> entry
  val update_ent : t -> path:Path.t -> entry -> t
  val sexp_of_t : t -> Sexp.t
end = struct
  type t = entry Path.Map.t

  let empty = Path.Map.empty

  let lookup_st tree_mem ~path ~label =
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

  let sexp_of_t tree_mem =
    let open Sexp_helper in
    l
      (Map.to_alist tree_mem
      |> List.map ~f:(fun (path, entry) ->
             l [ Path.sexp_of_t path; Value.sexp_of_entry entry ]))
end

module Phase = struct
  type t = phase

  let equal p1 p2 =
    match (p1, p2) with
    | P_init, P_init
    | P_update, P_update
    | P_retry, P_retry
    | P_effect, P_effect ->
        true
    | _, _ -> false

  let ( = ) = equal
  let ( <> ) p1 p2 = not (p1 = p2)
  let sexp_of_t = Value.sexp_of_phase
end

module Decision = struct
  type t = decision

  let equal d1 d2 =
    match (d1, d2) with
    | Idle, Idle | Retry, Retry | Update, Update -> true
    | _, _ -> false

  let ( = ) = equal
  let ( <> ) d1 d2 = not (d1 = d2)
  let sexp_of_t = Value.sexp_of_decision
end
