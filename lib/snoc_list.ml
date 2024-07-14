open! Core

type 'a t = [] | Snoc of 'a t * 'a

let ( ||> ) l a = Snoc (l, a)

let rec iter t ~(f : 'a -> unit) : unit =
  match t with
  | [] -> ()
  | Snoc (l, a) ->
      iter l ~f;
      f a

let to_list (l : 'a t) : 'a list =
  let[@tail_mod_cons] rec to_rev_list : 'a t -> 'a list = function
    | [] -> []
    | Snoc (l, a) -> a :: to_rev_list l
  in
  List.rev (to_rev_list l)

let of_list (l : 'a list) : 'a t =
  let[@tail_mod_cons] rec of_list_rev : 'a list -> 'a t = function
    | [] -> []
    | a :: l -> Snoc (of_list_rev l, a)
  in
  of_list_rev (List.rev l)

let fold t ~(init : 'acc) ~(f : 'acc -> 'a -> 'acc) : 'acc =
  match t with [] -> init | l -> to_list l |> List.fold ~init ~f

let sexp_of_t (sexp_of_a : 'a -> Sexp.t) (l : 'a t) : Sexp.t =
  List.sexp_of_t sexp_of_a (to_list l)

let t_of_sexp (a_of_sexp : Sexp.t -> 'a) (sexp : Sexp.t) : 'a t =
  of_list (List.t_of_sexp a_of_sexp sexp)
