module Typed = Soteria.Tiny_values.Typed
type symb_int = Typed.T.sint Typed.t
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)

open Symex.Syntax
open Typed.Infix
open Typed.Syntax
module StrMap = Map.Make (String)
type qos = symb_int StrMap.t
type call = { serv_name : string; args : symb_int list; qos : qos }

module Key = struct
  type t = Typed.T.sint Typed.t

  let compare = Typed.compare
  let sem_eq = Typed.sem_eq
  let simplify = Symex.simplify
  let pp = Typed.ppa
  let show v = v |> Typed.Expr.of_value |> Soteria.Tiny_values.Svalue.show
  let distinct = Typed.distinct
end

module ValMap = Soteria.Data.S_map.Make (Symex) (Key)
(*module ValMap = Map.Make (Soteria.Soteria_std.Int)*)
(* Each policy can specify to be checked only for portions of the history:
 1) when group_by is None, check for the whole history
 2) when it is Some p, check for all sub-sequences of the history where p,
the parameter, has been assigned the same symbolic value (skip all invoked services
 that do not have p as a parameter, group by p for the remaining services)
 For 2), the group_by must be aware of the path-condition, so Soteria.Data.Map is used.*)
              
type 'a checkerState =
  | Ungrouped of 'a
  | Grouped of string * 'a ValMap.t
             
type pChecker =
  | QosAggregate of symb_int checkerState * Contract.AST.binop (*to modify to cmp*) * Contract.AST.aggrop * string * int
  | QosAvg of (symb_int * int) checkerState * Contract.AST.binop * string * int
  | Dfa of int checkerState * (int -> string -> int) * int list
  | Ascending of symb_int checkerState * string
  | Descending of symb_int checkerState * string
            
(*the policy checker has a state that is updated at each invoke. If one update puts it in the final state, the policy is violated*)
let init_policy (policyType, groupBy) =
  let initial state =
    (match groupBy with
     | None -> Ungrouped state
     | Some param -> Grouped (param, ValMap.empty))
  in
  (match policyType with
   | Contract.AST.QosFieldOp (operator, Contract.AST.Avg, fieldName, i) ->
      QosAvg ((initial ((Typed.int 0), 0)), operator, fieldName, i)
   | Contract.AST.QosFieldOp (operator, aggregator, fieldName, i) ->
      (* meaning: <aggregator>(<fieldname>) <operator> i *)
      QosAggregate (
          (initial (Typed.int (match aggregator with
                                 | Contract.AST.Sum | Contract.AST.Avg -> 0
                                 | Contract.AST.Min -> Int.max_int
                                 | Contract.AST.Max -> Int.min_int))),
          operator, aggregator, fieldName, i)
   | Contract.AST.Regex reg ->
      Dfa
        ( (initial 0), (*current state*)
          (*TODO: placeholder dfa, needs to be replaced by the one obtained by the regex2dfa conversion*)
          (fun state service -> state),
          [] (*list of final states*)
        )
   | Contract.AST.Sort fieldName -> Ascending ((initial (Typed.int 0)), fieldName))

(*Warning: some policy may not be satisfied, but can be satisfied later.
  Ex: avg(cost) < 30, may not be satisfied when the costs are 35,30, but if the
  next invoke has cost = 3 it becomes satisfied. This also applies to sum(latency) > 50*)


let map_state initial f (c:call) (service:Contract.AST.service) = function
  | Ungrouped s ->
     let++ next = f s
     in Ungrouped next
  | Grouped (field, symMap) ->
     let idx = List.find_index (fun x -> x == field) (List.map fst service.params)
     in match idx with
        | None -> Symex.Result.ok (Grouped (field, symMap))
        | Some i ->
           let arg = List.nth c.args i
           in
           let* (k, s) = ValMap.find_opt arg symMap
           in
           let** next = match s with
             | None -> (f initial)
             | Some state -> (f state)
           in Symex.Result.ok (Grouped (field, (ValMap.syntactic_add k next symMap)))
              
let update_policy servMap (c:call) policy =
  let s = StrMap.find c.serv_name servMap
  in match policy with
     | QosAggregate (sint, cmp, aggrOp, aggrField, cmpInt) -> (*TODO*) Symex.Result.ok (QosAggregate (sint, cmp, aggrOp, aggrField, cmpInt))
     | QosAvg (sint_count, cmp, avgField, cmpInt) -> (*TODO*) Symex.Result.ok (QosAvg (sint_count, cmp, avgField, cmpInt))
     | Dfa (curState, transition, finalStates) ->
        let** result =
          map_state 0 (fun cur ->
              let nextState = transition cur c.serv_name
              in
              if List.mem nextState finalStates then
                Symex.Result.error "regex policy violation"
              else Symex.Result.ok nextState) c s curState
        in Symex.Result.ok (Dfa (result, transition, finalStates))
     | Ascending (maximum, field) -> (*TODO*) Symex.Result.ok (Ascending (maximum, field))
     | Descending (minimum, field) -> (*TODO*) Symex.Result.ok (Descending (minimum, field))
