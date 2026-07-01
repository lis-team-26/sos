open Symbolic.Runtime
open Symbolic.Data
open Expr.TypedAST
open Contract.AST
open Contract.TypedAST
open Reg2dfa
open Utils.Data
open Utils.Loc

(** Internal state of a policy checker. Each policy can specify to be checked
    only for some portions of the history:
    - when [group_by] is [None], check it against the whole history
    - when it is [Some param], check for all sub-sequences of the history where
      [param], the parameter, has been assigned the same symbolic value (skip
      all invoked services that do not have [param] as a parameter, group by
      [param] for the remaining services).

    For the second kind of policy, the [group_by] must be aware of the
    path-condition, so [Soteria.Data.Map] is used to remember the state of the
    policy verification for each symbolic value that has been assigned to
    [param] in every service invocation.*)
type 'a checker_state =
  | Ungrouped of 'a  (** Checks the policy against the whole history *)
  | Grouped of ident * 'a SymbolicMap.t
      (** Checks the policy against the sub-sequences of the history obtained by
          grouping by [param] *)

(** Encodes the kind of policy checker, which is determined by the policy
    specification. Policies kind are distinguished by their specification and by
    the way they are enforced during the execution. Each policy kind is also
    enriched with the information needed to enforce it. *)
type checker_kind =
  | QosAggregate of {
      initial_value : symb_int;  (** Initial value for the aggregation *)
      curr_state : symb_int checker_state;  (** Current aggregated value *)
      aggr_op : symb_int -> symb_int -> symb_int;  (** Aggregation operation *)
      field : ident;  (** QoS field to aggregate *)
      cmp_op : symb_int -> symb_int -> symb_bool;
          (** Comparison operation to use against the current aggregated value
              and the policy's threshold *)
      threshold : int;
          (** Threshold value for the comparison; depending on the comparison
              operation it can be a lower or upper bound *)
      verify_now : bool;
          (** Tells whether the policy should be verified at every invocation or
              at the end of the execution *)
    }
  | QosAvg of {
      curr_state : (symb_int * int) checker_state;
          (** Current sum of the aggregated QoS field with the number of
              invocations seen so far (needed to compute the average) *)
      cmp_op : symb_int -> symb_int -> symb_bool;
          (** Comparison operation to use against the current average and the
              policy's threshold *)
      field : ident;  (** QoS field to aggregate *)
      threshold : int;
          (** Threshold value for the final comparison against the computed
              average *)
    }
  | Dfa of {
      initial_state : Nfa.state;  (** Automaton's initial state*)
      curr_state : Nfa.state option checker_state;
          (** Current state of the automaton *)
      serv2letter : char StringMap.t;
          (** Mapping from service name to the character used in the regex *)
      transition : Nfa.state option -> char -> Nfa.state option;
          (** Automaton's transition relation *)
      final_states : Nfa.state list;
          (** Automaton's final states: if the current state is one of these,
              the policy is violated *)
    }
  | Ascending of {
      max : symb_int checker_state;
          (** Max value of the QoS field seen so far *)
      field : ident;  (** QoS field to check for ascending order *)
    }
  | Descending of {
      min : symb_int checker_state;
          (** Min value of the QoS field seen so far *)
      field : ident;  (** QoS field to check for descending order *)
    }

type policy_checker = { id : int; spec : policy_spec; checker : checker_kind }

let raise_violation ~loc policy =
  Symex.Result.error (located ~loc (PolicyError (policy.id, policy.spec)))

(** Returns true if the policy should be verified at every invocation, false
    otherwise. This is because some policy may not be satisfied now, but they
    can be satisfied later: for instance, [avg(cost) < 30] may not be satisfied
    when the costs are [[35;30]], but if the next invoke has [cost = 3] it
    becomes satisfied. This also applies to [sum(latency) > 50]. The two kind of
    policies must be distinguished: at each update, some of them can be verified
    now, and others can only be verified at the end. *)
let verifiable_now aggr_op cmp_op =
  let is_ascending = function Min -> false | _ -> true in
  let is_less = function Lt | Le -> true | _ -> false in
  is_ascending aggr_op == is_less cmp_op
  && match (aggr_op, cmp_op) with Avg, _ | _, Eq | _, Neq -> false | _ -> true

let cmp_fun_of_cmp_op = function
  | Lt -> Typed.lt
  | Le -> Typed.leq
  | Gt -> Typed.gt
  | Ge -> Typed.geq
  | Eq -> Typed.sem_eq
  | Neq -> fun x y -> Typed.not @@ Typed.sem_eq x y

let aggr_fun_of_aggr_op = function
  | Sum -> fun x y -> Typed.add x y
  | Max -> fun x y -> Typed.ite (Typed.gt y x) y x
  | Min -> fun x y -> Typed.ite (Typed.lt y x) y x
  | Avg -> failwith "Unreachable: average policies should be handled separately"

let build_policy_checker id policy_spec =
  let policy_type, group_by = policy_spec in
  let initialize_with state =
    match group_by with
    | None -> Ungrouped state
    | Some param -> Grouped (param, SymbolicMap.empty)
  in
  let checker =
    match policy_type with
    | QosFieldOp (Avg, field, cmp_op, threshold) ->
        QosAvg
          {
            curr_state = initialize_with (Typed.int 0, 0);
            cmp_op = cmp_fun_of_cmp_op cmp_op;
            field;
            threshold;
          }
    | QosFieldOp (aggr_op, field, cmp_op, threshold) ->
        (* Means that the policy must ensure the following: <aggr_op>(<field>) <cmp_op> <threshold> *)
        let initial_value =
          Typed.int
            (match aggr_op with
            | Sum | Avg -> 0
            | Min -> Int.max_int
            | Max -> Int.min_int)
        in
        QosAggregate
          {
            curr_state = initialize_with initial_value;
            initial_value;
            aggr_op = aggr_fun_of_aggr_op aggr_op;
            field;
            cmp_op = cmp_fun_of_cmp_op cmp_op;
            threshold;
            verify_now = verifiable_now aggr_op cmp_op;
          }
    | Regex (s2l, regex) ->
        let domain = CharSet.of_list @@ List.map snd s2l in
        let s2l = StringMap.of_seq @@ List.to_seq s2l in
        (* NOTE: this throws an exception if reg is malformed *)
        let dfa = Regex.reg2dfa ~domain regex in
        let step_if_in_domain maybe_curr letter =
          if not @@ CharSet.mem letter domain then maybe_curr
          else Dfa.step dfa maybe_curr letter
        in
        Dfa
          {
            initial_state = dfa.start;
            curr_state = initialize_with (Some dfa.start);
            serv2letter = s2l;
            transition = step_if_in_domain;
            final_states = Nfa.StateSet.to_list dfa.finals;
          }
    | Sort field -> Ascending { max = initialize_with (Typed.int 0); field }
  in
  { id; spec = policy_spec; checker }

(** Updates the current checker state by applying [f]. If the checker is
    ungrouped, the state is updated directly. If the checker is grouped, the
    state is updated for the group to which the argument belongs, which is
    initialized to [initial_state] if the group is new. *)
let update_checker_state ~initial_state ~args ~f = function
  | Ungrouped value ->
      let++ next_value = f value in
      Ungrouped next_value
  | Grouped (field, symb_map) -> (
      (* Get the symbolic value of the argument assigned to grouped parameter *)
      match StringMap.find_opt field args with
      (* If the service doesn't have that parameter, then skip the policy update *)
      | None -> Symex.Result.ok (Grouped (field, symb_map))
      | Some value ->
          let value =
            match value with
            | SymbInt i -> Typed.cast i
            | SymbBool b -> Typed.cast b
            | SymbReceipt _ ->
                failwith
                  "Unreachable: receipts cannot be used as grouping parameters"
          in
          (* Match it with previous argument assigned to [field], if any *)
          let* key, s = SymbolicMap.find_opt value symb_map in
          let** next_value =
            match s with None -> f initial_state | Some value -> f value
          in
          Symex.Result.ok
            (Grouped (field, SymbolicMap.syntactic_add key next_value symb_map))
      )

(** Updates the state of the current [policy] against the given [invocation]. If
    this can symbolically cause an error on some branches, they are reported. *)
let update_policy ~loc invocation policy =
  let** checker =
    match policy.checker with
    | QosAggregate checker -> (
        match StringMap.find_opt checker.field invocation.actual_qos with
        | Some (SymbInt value) ->
            let** next_state =
              update_checker_state ~initial_state:checker.initial_value
                ~args:invocation.actual_args
                ~f:(fun aggregate ->
                  let new_aggregate = checker.aggr_op aggregate value in
                  if checker.verify_now then
                    let policy_holds =
                      checker.cmp_op new_aggregate (Typed.int checker.threshold)
                    in
                    if%sat policy_holds then Symex.Result.ok new_aggregate
                    else raise_violation ~loc policy
                  else Symex.Result.ok new_aggregate)
                checker.curr_state
            in
            Symex.Result.ok
              (QosAggregate { checker with curr_state = next_state })
        | None -> failwith "Unreachable: QoS field not found in invocation"
        | _ ->
            failwith
              "Unreachable: expected integer QoS field for aggregate policy")
    | QosAvg checker -> (
        match StringMap.find_opt checker.field invocation.actual_qos with
        | Some (SymbInt value) ->
            let** next_state =
              update_checker_state
                ~initial_state:(Typed.int 0, 0)
                ~args:invocation.actual_args
                ~f:(fun (sum, cnt) ->
                  let new_cnt = cnt + 1 in
                  let new_sum = Typed.add sum value in
                  (* Don't check eagerly: avg can recover on future calls.
                       Just accumulate (sum, count) and defer the check to verify_policy. *)
                  Symex.Result.ok (new_sum, new_cnt))
                checker.curr_state
            in
            Symex.Result.ok (QosAvg { checker with curr_state = next_state })
        | None -> failwith "Unreachable: QoS field not found in invocation"
        | _ -> failwith "Expected integer QoS field for average policy")
    | Dfa checker ->
        let** next_state =
          update_checker_state ~initial_state:(Some checker.initial_state)
            ~args:invocation.actual_args
            ~f:(fun curr_state ->
              match
                StringMap.find_opt invocation.service.name checker.serv2letter
              with
              (* Service not in the regex domain: self-loop, state unchanged *)
              | None -> Symex.Result.ok curr_state
              | Some letter -> (
                  match checker.transition curr_state letter with
                  (* Ended up in a sink state *)
                  | None -> Symex.Result.ok None
                  | Some next_state ->
                      if List.mem next_state checker.final_states then
                        raise_violation ~loc policy
                      else Symex.Result.ok (Some next_state)))
            checker.curr_state
        in
        Symex.Result.ok (Dfa { checker with curr_state = next_state })
    | Ascending checker -> (
        match StringMap.find_opt checker.field invocation.actual_qos with
        | Some (SymbInt cv) ->
            let** new_max =
              update_checker_state ~initial_state:(Typed.int Int.min_int)
                ~args:invocation.actual_args
                ~f:(fun current_max ->
                  let violation = Typed.lt cv current_max in
                  if%sat violation then raise_violation ~loc policy
                  else Symex.Result.ok cv)
                checker.max
            in
            Symex.Result.ok (Ascending { checker with max = new_max })
        | None -> failwith "Unreachable: QoS field not found in invocation"
        | _ -> failwith "Expected integer QoS field for ascending policy")
    | Descending checker -> (
        match StringMap.find_opt checker.field invocation.actual_qos with
        | Some (SymbInt cv) ->
            let** new_min =
              update_checker_state ~initial_state:(Typed.int Int.max_int)
                ~args:invocation.actual_args
                ~f:(fun current_min ->
                  let violation = Typed.gt cv current_min in
                  if%sat violation then raise_violation ~loc policy
                  else Symex.Result.ok cv)
                checker.min
            in
            Symex.Result.ok (Descending { checker with min = new_min })
        | None -> failwith "Unreachable: QoS field not found in invocation"
        | _ -> failwith "Expected integer QoS field for descending policy")
  in
  Symex.Result.ok { policy with checker }

(** Helper function, which applies a check function [f] to every group's
    accumulated state. For [Ungrouped], applies [f] once to the single
    accumulated state. For [Grouped], it uses [SymbolicMap.find_opt] with a
    fresh symbolic key to let Soteria branch over all possible groups under
    their respective path conditions. *)
let check_each_group f = function
  | Ungrouped s -> f s
  | Grouped (_, symb_map) -> (
      (* [syntactic_bindings] returns the list of (key, value) pairs that have
         been inserted with [syntactic_add] — i.e. the concrete groups built
         during [update_policy]. We fold over them sequentially and stop at the
         first violation (the monadic bind propagates errors). *)
      let bindings = List.of_seq (SymbolicMap.syntactic_bindings symb_map) in
      match bindings with
      | [] ->
          (* No invocations were ever grouped: nothing to verify *)
          Symex.Result.ok ()
      | _ ->
          List.fold_left
            (fun acc (_, state) ->
              let** () = acc in
              f state)
            (Symex.Result.ok ()) bindings)

(** Verifies the final state of a policy checker without updating it. Called at
    the end of symbolic execution for policies that cannot be checked eagerly
    (i.e. [QosAggregate] with [verify_now = false], and [QosAvg]). Returns [()]
    if the policy is satisfied, an error message with [EOFLoc] as source code
    location otherwise. *)
let verify_policy policy =
  match policy.checker with
  | QosAggregate checker when not checker.verify_now ->
      check_each_group
        (fun aggregate ->
          (* [cmp_op] returns true when the policy IS satisfied, so negate for violation *)
          let satisfied =
            checker.cmp_op aggregate (Typed.int checker.threshold)
          in
          if%sat satisfied then Symex.Result.ok ()
          else raise_violation ~loc:EOFLoc policy)
        checker.curr_state
  | QosAvg checker ->
      (* [QosAvg] is never verified eagerly (avg can always recover across invocations) *)
      check_each_group
        (fun (sum, count) ->
          if count = 0 then Symex.Result.ok ()
          else
            let avg = Typed.div sum (Typed.nonzero count) in
            (* cmp returns true when the policy IS satisfied, so negate for violation *)
            let satisfied = checker.cmp_op avg (Typed.int checker.threshold) in
            if%sat satisfied then Symex.Result.ok ()
            else raise_violation ~loc:EOFLoc policy)
        checker.curr_state
  (* [Dfa], [Ascending] and [Descending] violations are monotone: if no violation occurred
     at any step, the final state is valid *)
  | _ -> Symex.Result.ok ()
