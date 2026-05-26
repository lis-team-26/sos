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
(* Each policy can specify to be checked only for portions of the history:
 1) when group_by is None, check for the whole history
 2) when it is Some p, check for all sub-sequences of the history where p,
the parameter, has been assigned the same symbolic value (skip all invoked services
 that do not have p as a parameter, group by p for the remaining services)
 For 2), the group_by must be aware of the path-condition, so Soteria.Data.Map is used
to remember the state of the policy verification for each symbolic value that has been
assigned to p in every service invocation.*)

type 'a checkerState =
  | Ungrouped of 'a (*whole history*)
  | Grouped of string (*=p*) * 'a ValMap.t
(*only those services that have p as parameter, invocations grouped by p*)

type pChecker =
  | QosAggregate of
      symb_int
      (*can be many things, depending on the aggregate operation*)
      checkerState
      * Expr.AST.bin_op (*comparison*)
      * Contract.AST.aggr_op (*sum, max, ...*)
      * string (*the Qos field to aggregate*)
      * int (*the integer to compare to the result of the aggregation*)
  | QosAvg of
      (symb_int (*sum on the Qos field*)
      * int (*count of service invocations seen so far*))
      checkerState
      * Expr.AST.bin_op (*comparison*)
      * string (*the Qos field to sum*)
      * int
    (*the integer to compare to the result of the sum divided by invoke count*)
  | Dfa of
      int (*state of the dfa, initially 0*) checkerState
      * (int -> string -> int)
      * (*transition relation*)
      int list (*list of final states*)
  | Ascending of
      symb_int (*max value of the Qos field seen so far*) checkerState
      * string (*the Qos field*)
  | Descending of
      symb_int (*min value of the Qos field seen so far*) checkerState
      * string (*the Qos field*)

(*the policy checker has a state that is updated at each invoke. If one update puts it in the final state, the policy is violated*)
let init_policy (policyType, groupBy) =
  let initial state =
    match groupBy with
    | None -> Ungrouped state
    | Some param -> Grouped (param, ValMap.empty)
  in
  match policyType with
  | Contract.AST.QosFieldOp (Contract.AST.Avg, fieldName, operator, i) ->
      QosAvg (initial (Typed.int 0, 0), operator, fieldName, i)
  | Contract.AST.QosFieldOp (aggregator, fieldName, operator, i) ->
      (* meaning: <aggregator>(<fieldname>) <operator> i *)
      QosAggregate
        ( initial
            (Typed.int
               (match aggregator with
               | Contract.AST.Sum | Contract.AST.Avg -> 0
               | Contract.AST.Min -> Int.max_int
               | Contract.AST.Max -> Int.min_int)),
          operator,
          aggregator,
          fieldName,
          i )
  | Contract.AST.Regex reg ->
      Dfa
        ( initial 0,
          (*current state*)
          (*TODO: placeholder dfa, needs to be replaced by the one obtained by the regex2dfa conversion*)
          (fun state service -> state),
          [] (*list of final states*) )
  | Contract.AST.Sort fieldName -> Ascending (initial (Typed.int 0), fieldName)

(*Warning: some policy may not be satisfied, but can be satisfied later.
  Ex: avg(cost) < 30, may not be satisfied when the costs are 35,30, but if the
  next invoke has cost = 3 it becomes satisfied. This also applies to sum(latency) > 50*)

let map_state initial f (c : call) (service : Contract.AST.service) = function
  | Ungrouped s ->
      let++ next = f s in
      Ungrouped next
  | Grouped (field, symMap) -> (
      let idx =
        List.find_index (fun x -> x == field) (List.map fst service.params)
      in
      match idx with
      | None ->
          Symex.Result.ok (Grouped (field, symMap))
          (*if the service doesn't have that parameter, then skip the invoke*)
      | Some i ->
          (*otherwise*)
          let arg =
            List.nth c.args
              i (*get the symbolic value of the argument assigned to p*)
          in
          let* k, s =
            ValMap.find_opt arg
              symMap (*match it with previous argument assigned to p, if any*)
          in
          let** next =
            match s with None -> f initial | Some state -> f state
          in
          Symex.Result.ok (Grouped (field, ValMap.syntactic_add k next symMap)))

let update_policy servMap (c : call) policy =
  let s = StrMap.find c.serv_name servMap in
  match policy with
  | QosAggregate (sint, cmp, aggrOp, aggrField, cmpInt) ->
      (*TODO*)
      Symex.Result.ok (QosAggregate (sint, cmp, aggrOp, aggrField, cmpInt))
  | QosAvg (sint_count, cmp, avgField, cmpInt) ->
      (*TODO*) Symex.Result.ok (QosAvg (sint_count, cmp, avgField, cmpInt))
  | Dfa (curState, transition, finalStates) ->
      let** result =
        map_state 0
          (fun cur ->
            let nextState = transition cur c.serv_name in
            if List.mem nextState finalStates then
              Symex.Result.error "regex policy violation"
            else Symex.Result.ok nextState)
          c s curState
      in
      Symex.Result.ok (Dfa (result, transition, finalStates))
  | Ascending (maximum, field) ->
      (*TODO*) Symex.Result.ok (Ascending (maximum, field))
  | Descending (minimum, field) ->
      (*TODO*) Symex.Result.ok (Descending (minimum, field))
