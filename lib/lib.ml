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
module PolicyChecker = PolicyChecker
                  
let parse src =
  let lexbuf = Lexing.from_channel (open_in src) in
  Parser.prog Lexer.read lexbuf

let symb_run ((contract : ContractAST.program), program) ~mode =
  (*create a map (name -> service)*)
  let serviceMap = List.fold_left (fun m (s : ContractAST.service) -> StrMap.add s.name s m) StrMap.empty contract.services
  in (*initialize each policy checker*)
  let policyInitStates = List.map (PolicyChecker.init_policy serviceMap) contract.policies
  in
  (SymbolicInterpreter.build_symb_process program policyInitStates serviceMap)
  |> SymbolicInterpreter.Symex.run ~mode
