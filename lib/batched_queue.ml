open! Core

module type S = sig
  type t
  type elt

  exception Empty_queue

  val empty : t
  val is_empty : t -> bool
  val enqueue : t -> elt -> t
  val front : t -> elt
  val dequeue : t -> t
  val size : t -> int
  val to_list : t -> elt list
  val fold : t -> init:'acc -> f:('acc -> elt -> 'acc) -> 'acc
  val iter : t -> f:(elt -> unit) -> unit
  val sexp_of_t : t -> Sexp.t
end

module M (T : sig
  type t

  val sexp_of_t : t -> Sexp.t
end) : S with type elt := T.t = struct
  type elt = T.t
  type t = { f : elt list; r : elt list }

  exception Empty_queue

  let empty = { f = []; r = [] }
  let is_empty = function { f = []; _ } -> true | _ -> false

  let enqueue q x =
    match q with
    | { f = []; _ } -> { f = [ x ]; r = [] }
    | { f; r } -> { f; r = x :: r }

  let front = function
    | { f = []; _ } -> raise Empty_queue
    | { f = h :: _; _ } -> h

  let dequeue = function
    | { f = []; _ } -> raise Empty_queue
    | { f = [ _ ]; r } -> { f = List.rev r; r = [] }
    | { f = _ :: t; r } -> { f = t; r }

  let size { f; r } = List.(length f + length r)
  let to_list { f; r } = f @ List.rev r
  let fold q = List.fold (to_list q)
  let iter q = List.iter (to_list q)
  let sexp_of_t t = List.sexp_of_t T.sexp_of_t (to_list t)
end
