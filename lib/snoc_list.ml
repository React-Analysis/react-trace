open! Base

type 'a t = [] | Snoc of 'a t * 'a

let ( ||> ) l a = Snoc (l, a)

let rec iter t ~(f : 'a -> unit) : unit =
  match t with
  | [] -> ()
  | Snoc (l, a) ->
      iter l ~f;
      f a

let[@tail_mod_cons] rec to_list : 'a t -> 'a list = function
  | [] -> []
  | Snoc (l, a) -> a :: to_list l

let[@tail_mod_cons] rec of_list : 'a list -> 'a t = function
  | [] -> []
  | a :: l -> Snoc (of_list l, a)
