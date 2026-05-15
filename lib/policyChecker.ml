module ContractAST = ContractAST
type pCheck =
  | QosAggregate of ContractAST.binop * ContractAST.aggrop * string * int
  | Dfa of int list * (int -> string -> int)
  | Sorted of string
            
type pChecker = {current : int; check : pCheck}

(*the policy checker has a state that is updated at each invoke. If one update puts it in the final state, the policy is violated*)
let init_policy serviceMap = function
  | ContractAST.QosFieldOp (operator, aggregator, fieldName, i) (* meaning: <aggregator>(<fieldname>) <operator> i*)
    -> {current = (match aggregator with
                   | ContractAST.Sum | ContractAST.Avg -> 0
                   | ContractAST.Min -> Int.max_int
                   | ContractAST.Max -> Int.min_int); check = QosAggregate (operator, aggregator, fieldName, i)}
  | ContractAST.Regex reg -> {current=0; check = Dfa ([](*list of final states*), (fun state service -> state) (*placeholder nfa, needs to be replaced by the one obtained by the regex2nfa conversion*))}
  | ContractAST.Sort fieldName -> {current=0; check = Sorted fieldName}
