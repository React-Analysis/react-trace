open! Base

type 'a t = { f : 'a list; r : 'a list }

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
