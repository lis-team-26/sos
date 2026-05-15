module Ast = Ast
module ContractAST = ContractAST
module ContractAST_pp = ContractAST_pp
module ContractLexer = ContractLexer
module ContractParser = ContractParser
module Lexer = Lexer
module Parser = Parser
module SymbolicInterpreter = SymbolicInterpreter
module Utils = Utils
module StrMap = Map.Make(String)
open PolicyChecker

(**)
let init_policy serviceMap = function
  | ContractAST.QosFieldOp (operator, aggregator, fieldName, i) (* meaning: <aggregator>(<fieldname>) <operator> i*)
    -> {current = (match aggregator with
                   | ContractAST.Sum | ContractAST.Avg -> 0
                   | ContractAST.Min -> Int.max_int
                   | ContractAST.Max -> Int.min_int); check = QosAggregate (operator, aggregator, fieldName, i)}
  | ContractAST.Regex reg -> {current=0; check = Nfa ([](*list of final states*), (fun state service -> state) (*placeholder nfa, needs to be replaced by the one obtained by the regex2nfa conversion*))}
  | ContractAST.Sort fieldName -> {current=0; check = Sorted fieldName}
                  
let parse src =
  let lexbuf = Lexing.from_channel (open_in src) in
  Parser.prog Lexer.read lexbuf

let symb_run ((contract : ContractAST.program), program) ~mode =
  (*create a map (name -> service)*)
  let serviceMap = List.fold_left (fun m (s : ContractAST.service) -> StrMap.add s.name s m) StrMap.empty contract.services
  in (*initialize each policy checker*)
  let policyInitStates = List.map (init_policy serviceMap) contract.policies
  in
  (SymbolicInterpreter.build_symb_process program policyInitStates serviceMap)
  |> SymbolicInterpreter.Symex.run ~mode
