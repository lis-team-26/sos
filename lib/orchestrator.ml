module AST = OrchestratorAST
module AST_pp = OrchestratorAST_pp
module Lexer = OrchestratorLexer
module Parser = OrchestratorParser

let parse src =
  let module Wrapper = Utils.MakeParser (struct
    type ast = AST.stmt
    type token = Parser.token

    exception LexerError = Lexer.LexerError
    exception ParserError = Parser.Error

    let pp = AST_pp.pp_program
    let lexer = Lexer.read
    let parser = Parser.program
  end) in
  Wrapper.parse src

module Utils = OrchestratorUtils

let symb_run ((contract : Contract.AST.contract), program) ~mode =
  let module StrMap = Map.Make (String) in
  (*create a map (name -> service)*)
  let serviceMap =
    List.fold_left
      (fun m (s : Contract.AST.service) -> StrMap.add s.name s m)
      StrMap.empty contract.services
  in
  (*initialize each policy checker*)
  let policyInitStates =
    List.map (PolicyChecker.init_policy serviceMap) contract.policies
  in
  SymbolicInterpreter.build_symb_process program policyInitStates serviceMap
  |> SymbolicInterpreter.Symex.run ~mode
