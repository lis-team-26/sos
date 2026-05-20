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
              
type 'a checkerState =
  | Ungrouped of 'a
  | Grouped of string * 'a ValMap.t
             
type pChecker =
  | QosAggregate of symb_int checkerState * Contract.AST.binop (*to modify to cmp*) * Contract.AST.aggrop * string * int
  | QosAvg of (symb_int * int) checkerState * Contract.AST.binop * string * int
  | Dfa of int checkerState * (int -> string -> int) * int list
  | Ascending of symb_int checkerState * string (*Only works with constants, like thrust*)
  | Descending of symb_int checkerState * string (*Only works with constants, like thrust*)
            
(*the policy checker has a state that is updated at each invoke. If one update puts it in the final state, the policy is violated*)
let init_policy (policyType, _ (*for now, assume groupBy == None*)) =
      (match policyType with
       | Contract.AST.QosFieldOp (operator, Contract.AST.Avg, fieldName, i) ->
          QosAvg ((Ungrouped ((Typed.int 0), 0)), operator, fieldName, i)
       | Contract.AST.QosFieldOp (operator, aggregator, fieldName, i) ->
          (* meaning: <aggregator>(<fieldname>) <operator> i *)
          QosAggregate (
              (Ungrouped (Typed.int (match aggregator with
                         | Contract.AST.Sum | Contract.AST.Avg -> 0
                         | Contract.AST.Min -> Int.max_int
                         | Contract.AST.Max -> Int.min_int))),
              operator, aggregator, fieldName, i)
       | Contract.AST.Regex reg ->
          Dfa
            ( (Ungrouped 0), (*current state*)
              (*placeholder dfa, needs to be replaced by the one obtained by the regex2dfa conversion*)
              (fun state service -> state),
              [] (*list of final states*)
            )
       | Contract.AST.Sort fieldName -> Ascending ((Ungrouped (Typed.int 0)), fieldName))

(*Warning: some policy may not be satisfied, but can be satisfied later.
  Ex: avg(cost) < 30, may not be satisfied when the costs are 35,30, but if the
  next invoke has cost = 3 it becomes satisfied. This also applies to sum(latency) > 50*)
