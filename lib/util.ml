open! Base

let pad_or_truncate (lst : 'a list) ~(len : int) : 'a option list =
  let open List in
  let l = length lst in
  let pad = if l < len then init (len - l) ~f:(fun _ -> None) else [] in
  let lst = take lst len in
  map ~f:(fun x -> Some x) lst @ pad

let fold2i_exn (l1 : 'a list) (l2 : 'b list) ~(init : 'acc)
    ~(f : int -> 'acc -> 'a -> 'b -> 'acc) : 'acc =
  snd
    (List.fold2_exn l1 l2 ~init:(0, init) ~f:(fun (i, acc) v1 v2 ->
         (i + 1, f i acc v1 v2)))
