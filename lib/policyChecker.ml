module Typed = Soteria.Tiny_values.Typed
type symb_int = Typed.T.sint Typed.t
(*module ValMap = Soteria.Data.S_map.Make (Soteria.Tiny_values.Svalue) (*can't make it work...*)*)
module ValMap = Map.Make (Soteria.Soteria_std.Int)
(* Each policy can specify to be checked only for portions of the history:
 1) when group_by is None, check for the whole history
 2) when it is Some p, check for all sub-sequences of the history where p,
the parameter, has been assigned the same symbolic value (skip all invoked services
 that do not have p as a parameter, group by p for the remaining services)
 For 2), the group_by must be aware of the path-condition, so Soteria.Data.Map is used.
 For 1), just use a singleton 0 -> state *)
              

type pCheck =
  | QosAggregate of symb_int ValMap.t * Contract.AST.binop * Contract.AST.aggrop * string * int
  | QosAvg of (symb_int * int) ValMap.t * Contract.AST.binop * string * int
  | Dfa of int ValMap.t * (int -> string -> int) * int list
  | Sorted of int ValMap.t * string (*Only works with constants, like thrust*)

type pChecker = { check: pCheck; group_by: string option}
            
(*the policy checker has a state that is updated at each invoke. If one update puts it in the final state, the policy is violated*)
let init_policy (policyType, _ (*for now, assume groupBy == None*)) =
  { check =
      (match policyType with
       | Contract.AST.QosFieldOp (operator, Contract.AST.Avg, fieldName, i) ->
          QosAvg (ValMap.singleton ((*Typed.int*) 0) ((Typed.int 0),0), operator, fieldName, i)
       | Contract.AST.QosFieldOp (operator, aggregator, fieldName, i) ->
          (* meaning: <aggregator>(<fieldname>) <operator> i *)
          QosAggregate (
              ValMap.singleton ((*Typed.int*) 0) (Typed.int (match aggregator with
                         | Contract.AST.Sum | Contract.AST.Avg -> 0
                         | Contract.AST.Min -> Int.max_int
                         | Contract.AST.Max -> Int.min_int)),
              operator, aggregator, fieldName, i)
       | Contract.AST.Regex reg ->
          Dfa
            ( ValMap.singleton ((*Typed.int*) 0) ((*Typed.int*) 0), (*current state*)
              (*placeholder dfa, needs to be replaced by the one obtained by the regex2dfa conversion*)
              (fun state service -> state),
              [] (*list of final states*)
            )
       | Contract.AST.Sort fieldName -> Sorted ((ValMap.singleton 0 0), fieldName));
    group_by = None }
