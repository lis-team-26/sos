module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)
module CharSet = Set.Make (Char)
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type ident = string
(** Type of identificators *)

type 'a env = 'a StringMap.t
(** A mapping from variable names to their values *)

(** Function composition operator *)
let ( >> ) f g = fun x -> g (f x)
