open! Base

type 'a t = [] | Snoc of 'a t * 'a

let ( ||> ) l a = Snoc (l, a)

let rec iter t ~(f : 'a -> unit) : unit =
  match t with
  | [] -> ()
  | Snoc (l, a) ->
      f a;
      iter l ~f
