type pCheck =
  | QosAggregate of Contract.AST.binop * Contract.AST.aggrop * string * int
  | Dfa of int list * (int -> string -> int)
  | Sorted of string

type pChecker = { current : int; check : pCheck }

(*the policy checker has a state that is updated at each invoke. If one update puts it in the final state, the policy is violated*)
let init_policy serviceMap = function
  | Contract.AST.QosFieldOp (operator, aggregator, fieldName, i)
  (* meaning: <aggregator>(<fieldname>) <operator> i*) ->
      {
        current =
          (match aggregator with
          | Contract.AST.Sum | Contract.AST.Avg -> 0
          | Contract.AST.Min -> Int.max_int
          | Contract.AST.Max -> Int.min_int);
        check = QosAggregate (operator, aggregator, fieldName, i);
      }
  | Contract.AST.Regex reg ->
      {
        current = 0;
        check =
          Dfa
            ( [] (*list of final states*),
              fun state service -> state
              (*placeholder nfa, needs to be replaced by the one obtained by the regex2nfa conversion*)
            );
      }
  | Contract.AST.Sort fieldName -> { current = 0; check = Sorted fieldName }
