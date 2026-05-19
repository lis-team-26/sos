module Typed = Soteria.Tiny_values.Typed
type symb_int = Typed.T.sint Typed.t

type pChecker =
  | QosAggregate of symb_int * Contract.AST.binop * Contract.AST.aggrop * string * int
  | QosAvg of symb_int * int * Contract.AST.binop * string * int
  | Dfa of int * (int -> string -> int) * int list
  | Sorted of int * string (*Only works with constants, like thrust*)

(*the policy checker has a state that is updated at each invoke. If one update puts it in the final state, the policy is violated*)
let init_policy serviceMap = function
  | Contract.AST.QosFieldOp (operator, Contract.AST.Avg, fieldName, i) ->
     QosAvg (Typed.int 0, 0, operator, fieldName, i)
  | Contract.AST.QosFieldOp (operator, aggregator, fieldName, i) ->
     (* meaning: <aggregator>(<fieldname>) <operator> i *)
     QosAggregate (
         Typed.int (match aggregator with
                    | Contract.AST.Sum | Contract.AST.Avg -> 0
                    | Contract.AST.Min -> Int.max_int
                    | Contract.AST.Max -> Int.min_int),
         operator, aggregator, fieldName, i)
  | Contract.AST.Regex reg ->
     Dfa
       ( 0,
         (*placeholder nfa, needs to be replaced by the one obtained by the regex2nfa conversion*)
         (fun state service -> state),
         [] (*list of final states*)
       )
  | Contract.AST.Sort fieldName -> Sorted (0, fieldName)
