open Symbolic.Runtime
open Symbolic.Data
open Expr.TypedAST
open Contract.AST
open Contract.TypedAST
open Reg2dfa
open Utils.Data

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
  | Grouped of string (*=p*) * 'a SymbolicMap.t
(*only those services that have p as parameter, invocations grouped by p*)

type pChecker =
  | QosAggregate of
      symb_int
      (*can be many things, depending on the aggregate operation*)
      checkerState
      * symb_int (*initial state*)
      * (symb_int -> symb_int -> symb_int)
      * (*sum, max, ...*)
        string
        (*the Qos field to aggregate*)
      * (symb_int -> symb_int -> symb_bool)
      * (*comparison*)
        int
        (*the integer to compare to the result of the aggregation*)
      * bool (*should the policy be verified each time?*)
  | QosAvg of
      (symb_int (*sum on the Qos field*)
      * int (*count of service invocations seen so far*))
      checkerState
      * (symb_int -> symb_int -> symb_bool)
      * (*comparison*)
        string
        (*the Qos field to sum*)
      * int
    (*the integer to compare to the result of the sum divided by invoke count*)
  | Dfa of
      Nfa.state (*initial state of the dfa*)
      * Nfa.state option (*current state of the policy checker*) checkerState
      * char StringMap.t
      * (Nfa.state option -> char -> Nfa.state option)
      * (*transition relation*)
      Nfa.state list (*list of final states*)
  | Ascending of
      symb_int (*max value of the Qos field seen so far*) checkerState
      * string (*the Qos field*)
  | Descending of
      symb_int (*min value of the Qos field seen so far*) checkerState
      * string (*the Qos field*)

type policyChecker = { id : int; policy : policy; checker : pChecker }

(*Warning: some policy may not be satisfied, but can be satisfied later.
  Ex: avg(cost) < 30, may not be satisfied when the costs are 35,30, but if the
  next invoke has cost = 3 it becomes satisfied. This also applies to sum(latency) > 50.
 The distinction must be made between these two kind of policies: at each update, some
 of them can be verified now, other can only be verified at the end*)
let verify_now aggrOp cmp =
  let ascending = function Min -> false | _ -> true in
  let less = function Lt | Le -> true | _ -> false in
  ascending aggrOp == less cmp
  && match (aggrOp, cmp) with Avg, _ | _, Eq | _, Neq -> false | _ -> true

let cmp_function = function
  | Lt -> Typed.lt
  | Le -> Typed.leq
  | Gt -> Typed.gt
  | Ge -> Typed.geq
  | Eq -> Typed.sem_eq
  | Neq -> fun l r -> Typed.not @@ Typed.sem_eq l r

let aggr_function = function
  | Sum -> fun acc cv -> Typed.add acc cv
  | Max -> fun acc cv -> Typed.ite (Typed.gt cv acc) cv acc
  | Min -> fun acc cv -> Typed.ite (Typed.lt cv acc) cv acc
  | Avg -> failwith "Avg should be handled separately"

(*the policy checker has a state that is updated at each invoke. If one update puts it in the final state, the policy is violated*)
let init_policy id (policyType, group_by) =
  let initial_state state =
    match group_by with
    | None -> Ungrouped state
    | Some param -> Grouped (param, SymbolicMap.empty)
  in
  let checker =
    match policyType with
    | QosFieldOp (Avg, fieldName, operator, i) ->
        QosAvg
          (initial_state (Typed.int 0, 0), cmp_function operator, fieldName, i)
    | QosFieldOp (aggregator, fieldName, operator, i) ->
        (* meaning: <aggregator>(<fieldname>) <operator> i *)
        let init =
          Typed.int
            (match aggregator with
            | Sum | Avg -> 0
            | Min -> Int.max_int
            | Max -> Int.min_int)
        in
        QosAggregate
          ( initial_state init,
            init,
            aggr_function aggregator,
            fieldName,
            cmp_function operator,
            i,
            verify_now aggregator operator )
    | Regex (serv2chr, reg) ->
        let domain = CharSet.of_list @@ List.map snd serv2chr in
        let serv2chr = StringMap.of_seq @@ List.to_seq serv2chr in
        (*NOTE: this throws an exception if reg is malformed*)
        let dfa = Regex.reg2dfa ~domain reg in
        let step_if_in_domain =
         fun maybe_cur ch ->
          if not @@ CharSet.mem ch domain then maybe_cur
          else Dfa.step dfa maybe_cur ch
        in
        Dfa
          ( dfa.start,
            initial_state (Some dfa.start),
            (*current state*)
            serv2chr,
            (*mapping from service name -> char*)
            step_if_in_domain,
            Nfa.StateSet.to_list dfa.finals )
    | Sort fieldName -> Ascending (initial_state (Typed.int 0), fieldName)
  in
  { id; policy = (policyType, group_by); checker }

let map_state initial f invocation service = function
  | Ungrouped s ->
      let++ next = f s in
      Ungrouped next
  | Grouped (field, symMap) -> (
      let v =
        (*get the symbolic value of the argument assigned to p*)
        StringMap.find_opt field invocation.actual_args
      in
      match v with
      | None ->
          (*if the service doesn't have that parameter, then skip the invoke*)
          Symex.Result.ok (Grouped (field, symMap))
      | Some ar ->
          let arg =
            match ar with
            | SymbInt i -> Typed.cast i
            | SymbBool b -> Typed.cast b
            | SymbReceipt _ ->
                failwith
                  "Unreachable: receipts cannot be used as grouping parameters"
          in
          (*otherwise*)
          let* k, s =
            SymbolicMap.find_opt arg
              symMap (*match it with previous argument assigned to p, if any*)
          in
          let** next =
            match s with None -> f initial | Some state -> f state
          in
          Symex.Result.ok
            (Grouped (field, SymbolicMap.syntactic_add k next symMap)))

let update_policy invocation policy =
  let svc = invocation.service in
  match policy.checker with
  | QosAggregate (sint, initial, aggrOp, aggrField, cmp, cmpInt, verNow) -> (
      match StringMap.find_opt aggrField invocation.actual_qos with
      | None -> Symex.Result.ok policy
      | Some current_val -> (
          match current_val with
          | SymbInt cv ->
              let** next =
                map_state initial
                  (fun aggregate ->
                    let new_aggregate = aggrOp aggregate cv in
                    if verNow then
                      let policy_holds = cmp new_aggregate (Typed.int cmpInt) in
                      if%sat policy_holds then Symex.Result.ok new_aggregate
                      else
                        Symex.Result.error (Policy (policy.id, policy.policy))
                    else Symex.Result.ok new_aggregate)
                  invocation svc sint
              in
              Symex.Result.ok
                {
                  policy with
                  checker =
                    QosAggregate
                      (next, initial, aggrOp, aggrField, cmp, cmpInt, verNow);
                }
          | _ -> failwith "Expected integer QoS field for aggregate policy"))
  | QosAvg (sint_count, cmp, avgField, cmpInt) -> (
      match StringMap.find_opt avgField invocation.actual_qos with
      | None ->
          (* Service doesn't report this QoS field; skip policy update *)
          Symex.Result.ok policy
      | Some current_val -> (
          match current_val with
          | SymbInt cv ->
              let** result =
                map_state
                  (Typed.int 0, 0)
                  (fun value ->
                    let new_cnt = snd value + 1 in
                    let new_sum = Typed.add (fst value) cv in
                    Symex.Result.ok (new_sum, new_cnt))
                  invocation svc sint_count
              in
              Symex.Result.ok
                { policy with checker = QosAvg (result, cmp, avgField, cmpInt) }
          | _ -> failwith "Expected integer QoS field for average policy"))
  | Dfa (start, curState, servMap, transition, finalStates) ->
      let** result =
        map_state (Some start)
          (fun cur ->
            let chr_opt = StringMap.find_opt svc.name servMap in
            match chr_opt with
            | None -> Symex.Result.ok cur (*ignore service not in servMap*)
            | Some chr -> (
                let nextState = transition cur chr in
                match nextState with
                (*Ended up in sink state*)
                | None -> Symex.Result.ok nextState
                | Some nextState ->
                    if List.mem nextState finalStates then
                      Symex.Result.error (Policy (policy.id, policy.policy))
                    else Symex.Result.ok (Some nextState)))
          invocation svc curState
      in
      Symex.Result.ok
        {
          policy with
          checker = Dfa (start, result, servMap, transition, finalStates);
        }
  | Ascending (maximum, field) -> (
      match StringMap.find_opt field invocation.actual_qos with
      | None -> Symex.Result.ok policy
      | Some current_val -> (
          match current_val with
          | SymbInt cv ->
              let** next =
                map_state (Typed.int Int.min_int)
                  (fun current_max ->
                    let violation = Typed.lt cv current_max in
                    if%sat violation then
                      Symex.Result.error (Policy (policy.id, policy.policy))
                    else Symex.Result.ok cv)
                  invocation svc maximum
              in
              Symex.Result.ok { policy with checker = Ascending (next, field) }
          | _ -> failwith "Expected integer QoS field for ascending policy"))
  | Descending (minimum, field) -> (
      match StringMap.find_opt field invocation.actual_qos with
      | None -> Symex.Result.ok policy
      | Some current_val -> (
          match current_val with
          | SymbInt cv ->
              let** next =
                map_state (Typed.int Int.max_int)
                  (fun current_min ->
                    let violation = Typed.gt cv current_min in
                    if%sat violation then
                      Symex.Result.error (Policy (policy.id, policy.policy))
                    else Symex.Result.ok cv)
                  invocation svc minimum
              in
              Symex.Result.ok { policy with checker = Descending (next, field) }
          | _ -> failwith "Expected integer QoS field for descending policy"))
