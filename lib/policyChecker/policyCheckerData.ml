open Symbolic.Runtime
open Symbolic.Data
open Contract.TypedAST
open Reg2dfa
open Utils.Data

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
