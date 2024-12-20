open! Base

let pad_or_truncate (lst : 'a list) ~(len : int) : 'a option list =
  let open List in
  let l = length lst in
  let pad = if l < len then init (len - l) ~f:(fun _ -> None) else [] in
  let lst = take lst len in
  map ~f:(fun x -> Some x) lst @ pad
