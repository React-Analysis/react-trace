open! Base

module Map_key (T : sig
  type t
  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end) =
struct
  include T

  module Map = struct
    open Map
    include M (T)

    let empty = empty (module T)
  end
end
