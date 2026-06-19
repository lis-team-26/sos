(* nfa.ml *)
type state = int32

module StateSet = Set.Make (Int32)
module CharMap = Map.Make (Char)

type transitions = StateSet.t CharMap.t

type nfa = {
  start : StateSet.t;
  finals : StateSet.t;
  next : state -> transitions;
}
