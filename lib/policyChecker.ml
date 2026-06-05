module Typed = Soteria.Tiny_values.Typed

type symb_int = Typed.T.sint Typed.t

module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
open Symex.Syntax
open Typed.Infix
open Typed.Syntax
module StrMap = Map.Make (String)
module CharSet = Set.Make(Char)

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
      * Contract.AST.aggrop (*sum, max, ...*)
      * string (*the Qos field to aggregate*)
      * (Typed.T.sint Typed.t -> Typed.T.sint Typed.t -> Typed.sbool Typed.t ) (*comparison*)
      * int (*the integer to compare to the result of the aggregation*)
      * bool (*should the policy be verified each time?*)
  | QosAvg of
      (symb_int (*sum on the Qos field*)
      * int (*count of service invocations seen so far*))
      checkerState
      * (Typed.T.sint Typed.t -> Typed.T.sint Typed.t -> Typed.sbool Typed.t ) (*comparison*)
      * string (*the Qos field to sum*)
      * int
    (*the integer to compare to the result of the sum divided by invoke count*)
  | Dfa of
      int32 (*initial state of the dfa*)
      * int32 option (*current state of the policy checker*) checkerState
      * char StrMap.t
      * (int32 option -> char -> int32 option)
      * (*transition relation*)
      int32 list (*list of final states*)
  | Ascending of
      symb_int (*max value of the Qos field seen so far*) checkerState
      * string (*the Qos field*)
  | Descending of
      symb_int (*min value of the Qos field seen so far*) checkerState
      * string (*the Qos field*)

(*Warning: some policy may not be satisfied, but can be satisfied later.
  Ex: avg(cost) < 30, may not be satisfied when the costs are 35,30, but if the
  next invoke has cost = 3 it becomes satisfied. This also applies to sum(latency) > 50.
 The distinction must be made between these two kind of policies: at each update, some
 of them can be verified now, other can only be verified at the end*)
let verify_now aggrOp cmp =
    let ascending = function
      | Contract.AST.Min -> false
      | _ -> true
    in
    let less = function
      | Contract.AST.Lt | Contract.AST.Le -> true
      | _ -> false
    in
    (ascending aggrOp) == (less cmp) && (
      match (aggrOp, cmp) with
      | (Contract.AST.Avg, _) | (_, Contract.AST.Eq) | (_, Contract.AST.Neq) -> false
      | _ -> true
    )

let cmp_function = function
  | Contract.AST.Lt -> Typed.lt
  | Contract.AST.Le -> Typed.leq
  | Contract.AST.Gt -> Typed.gt
  | Contract.AST.Ge -> Typed.geq
  | Contract.AST.Eq -> Typed.sem_eq
  | Contract.AST.Neq -> (fun l r -> Typed.not @@ Typed.sem_eq l r)
  | _ -> failwith "Unknown comparison operator"

    (*the policy checker has a state that is updated at each invoke. If one update puts it in the final state, the policy is violated*)
let init_policy (policyType, groupBy) =
  let initial state =
    match groupBy with
    | None -> Ungrouped state
    | Some param -> Grouped (param, ValMap.empty)
  in
  match policyType with
  | Contract.AST.QosFieldOp (Contract.AST.Avg, fieldName, operator, i) ->
      QosAvg (initial (Typed.int 0, 0), (cmp_function operator), fieldName, i)
  | Contract.AST.QosFieldOp (aggregator, fieldName, operator, i) ->
      (* meaning: <aggregator>(<fieldname>) <operator> i *)
      QosAggregate
        ( initial
            (Typed.int
               (match aggregator with
               | Contract.AST.Sum | Contract.AST.Avg -> 0
               | Contract.AST.Min -> Int.max_int
               | Contract.AST.Max -> Int.min_int)),
          aggregator,
          fieldName,
          (cmp_function operator),
          i,
          (verify_now aggregator operator))
  | Contract.AST.Regex (serv2chr, reg) ->
      let open Reg2dfa in
      let domain = CharSet.of_list @@ List.map snd serv2chr in
      let serv2chr =  StrMap.of_seq @@ List.to_seq serv2chr in
      (*NOTE: this throws an exception if reg is malformed*)
      let dfa = Reg2dfa.Regex.reg2dfa ~domain reg in
      let step_if_in_domain = 
        fun maybe_cur ch ->
            if not @@ CharSet.mem ch domain then maybe_cur 
            else
              Dfa.step dfa maybe_cur ch
      in
      Dfa
        ( dfa.start,
          initial (Some dfa.start), (*current state*)
          serv2chr, (*mapping from service name -> char*)
          step_if_in_domain,
          Nfa.StateSet.to_list dfa.finals)
  | Contract.AST.Sort fieldName -> Ascending (initial (Typed.int 0), fieldName)


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
  | QosAggregate (sint, aggrOp, aggrField, cmp, cmpInt, verNow) ->
      let current_val = StrMap.find aggrField c.qos in

      let apply_aggregator acc =
        match aggrOp with
        | Contract.AST.Sum -> Typed.add acc current_val
        | Contract.AST.Max -> 
            Typed.ite
              (Typed.gt current_val acc)
              current_val
              acc
        | Contract.AST.Min -> 
            Typed.ite
              (Typed.lt current_val acc)
              current_val
              acc
        | Contract.AST.Avg -> failwith "Avg should be handled separately"
                                       (*| _ -> failwith "Unknown aggregator"*)
      in

      let** next =
        map_state (
          match aggrOp with 
          | Contract.AST.Sum -> Typed.int 0 
          | Contract.AST.Min -> Typed.int Int.max_int
          | Contract.AST.Max -> Typed.int Int.min_int
          | Contract.AST.Avg -> assert false )
          (fun aggregate ->
            let new_aggregate = apply_aggregator aggregate in
            if verNow then
              let violation = cmp new_aggregate (Typed.int cmpInt) in
              Symex.branch_on violation 
                ~then_: (fun () -> Symex.Result.error "aggregate policy violation")
                ~else_: (fun () -> Symex.Result.ok new_aggregate)
            else Symex.Result.ok new_aggregate)
          c s sint

      in 
      Symex.Result.ok (QosAggregate (sint, aggrOp, aggrField, cmp, cmpInt, verNow))
  | QosAvg (sint_count, cmp, avgField, cmpInt) ->
    let current_val = StrMap.find avgField c.qos in
    let** result = map_state (Typed.int 0, 0)
    ( fun value -> let new_cnt = (snd value) + 1 in 
      let new_val = Typed.div (Typed.add (fst value) current_val) (Typed.nonzero new_cnt) in 
      let violated = cmp new_val (Typed.int cmpInt) in
      Symex.branch_on violated 
              ~then_: (fun () -> Symex.Result.error "average policy violation")
              ~else_: (fun () -> Symex.Result.ok (new_val,new_cnt))
      ) c s sint_count in
      Symex.Result.ok (QosAvg (result, cmp, avgField, cmpInt))
  | Dfa (start,curState, servMap, transition, finalStates) ->
      let** result =
        map_state (Some start)
          (fun cur ->
            let chr_opt = StrMap.find_opt c.serv_name servMap
            in
            match chr_opt with
            (* Ignore services not in domain *)
            | None -> Symex.Result.ok cur
            | Some chr ->
               let nextState = transition cur chr in
               match nextState with
               (*Ended up in sink state*)
               | None -> Symex.Result.ok nextState
               | Some nextState -> 
                if List.mem nextState finalStates then
                 Symex.Result.error "regex policy violation"
                else Symex.Result.ok (Some nextState))
          c s curState
      in
      Symex.Result.ok (Dfa (start,result, servMap, transition, finalStates))
  | Ascending (maximum, field) ->
      let current_val = StrMap.find field c.qos in
      let** next =
        map_state (Typed.int Int.min_int)
          (fun current_max ->
            let violation = Typed.lt current_val current_max in
            Symex.branch_on violation 
              ~then_: (fun () -> Symex.Result.error "ascending policy violation: value decreased")
              ~else_: (fun () -> Symex.Result.ok current_val)
            )
          c s maximum
      in
      Symex.Result.ok (Ascending (next, field))
  | Descending (minimum, field) ->
      let current_val = StrMap.find field c.qos in
      let** next =
        map_state (Typed.int Int.max_int)
          (fun current_min ->
            let violation = Typed.gt current_val current_min in
            Symex.branch_on violation 
              ~then_: (fun () -> Symex.Result.error "descending policy violation: value increased")
              ~else_: (fun () -> Symex.Result.ok current_val)
            )
          c s minimum
      in
      Symex.Result.ok (Descending (next, field))

(* Verify the final state of a policy checker without updating it.
Called at the end of symbolic execution for policies that cannot be
checked eagerly (i.e. QosAggregate with verNow = false, and QosAvg).
Returns Symex.Result.ok () if the policy is satisfied,
Symex.Result.error msg otherwise. *)

(* Helper: apply a check function to every group's accumulated state.
   For Ungrouped, applies f once to the single accumulated state.
   For Grouped, uses ValMap.find_opt with a fresh symbolic key to let Soteria
   branch over all possible groups under their respective path conditions. *)

let check_each_group f = function
  | Ungrouped s -> f s
  | Grouped (_, symMap) ->
      let* key = Symex.nondet Typed.t_int in
      let* _, s = ValMap.find_opt key symMap in
      (match s with
      | None -> Symex.Result.ok ()   (* no invocations in this group *)
      | Some state -> f state)

let verify_policy = function
  | QosAggregate (sint, _, _, cmp, cmpInt, verNow) ->
      (* Already verified eagerly at each step when verNow = true *)
      if verNow then Symex.Result.ok ()
      else
        check_each_group (fun aggregate ->
          let violation = cmp aggregate (Typed.int cmpInt) in
          Symex.branch_on violation
            ~then_:(fun () -> Symex.Result.error "aggregate policy violation")
            ~else_:(fun () -> Symex.Result.ok ()))
          sint
  | QosAvg (sint_count, cmp, _, cmpInt) ->
      (* QosAvg is never verified eagerly (avg can recover across invocations) *)
      check_each_group (fun (sum, count) ->
        if count = 0 then Symex.Result.ok ()
        else
          let avg = Typed.div sum (Typed.nonzero count) in
          let violation = cmp avg (Typed.int cmpInt) in
          Symex.branch_on violation
            ~then_:(fun () -> Symex.Result.error "average policy violation")
            ~else_:(fun () -> Symex.Result.ok ()))
        sint_count
  (* Dfa, Ascending, Descending: violations are monotone.
     If no violation occurred at any step, the final state is valid. *)
  | Dfa _ | Ascending _ | Descending _ -> Symex.Result.ok ()
