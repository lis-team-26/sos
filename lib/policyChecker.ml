type pCheck =
  | QosAggregate of ContractAST.binop * ContractAST.aggrop * string * int
  | Nfa of int list * (int -> string -> int)
  | Sorted of string
            
type pChecker = {current : int; check : pCheck}
