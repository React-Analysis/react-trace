open! Base

module Map_key (T : sig
  type t

  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
  val of_string : string -> t
  val to_string : t -> string
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val compare : t -> t -> int
  val min : t -> t -> t
  val max : t -> t -> t
  val ascending : t -> t -> int
  val descending : t -> t -> int
  val between : t -> low:t -> high:t -> bool
  val clamp_exn : t -> min:t -> max:t -> t
  val clamp : t -> min:t -> max:t -> t Or_error.t

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
  val hash : t -> int
  val equal : t -> t -> bool
end) =
struct
  include T

  module Map = struct
    open Map
    include M (T)

    let empty = empty (module T)
  end
end

let pad_or_truncate (lst : 'a list) ~(len : int) : 'a option list =
  let open List in
  let l = length lst in
  let pad = if l < len then init (len - l) ~f:(fun _ -> None) else [] in
  let lst = take lst len in
  map ~f:(fun x -> Some x) lst @ pad
