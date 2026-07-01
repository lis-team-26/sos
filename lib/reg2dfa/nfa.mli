type state = int32

module StateSet : Set.S with type elt = int32
module CharMap : Map.S with type key = char

type transitions = StateSet.t CharMap.t

type nfa = {
  start : StateSet.t;  (** The start states. *)
  finals : StateSet.t;  (** The final (or "accept") states. *)
  next : state -> transitions;
      (** The transition function, that maps a state and a character to a set of
          states. *)
}
