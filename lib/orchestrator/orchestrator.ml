open Contract.TypedAST
open Utils.Parser
module AST = OrchestratorAST
module AST_pp = OrchestratorAST_pp
module TypedAST = TypedOrchestratorAST
module TypedAST_pp = TypedOrchestratorAST_pp
module Lexer = OrchestratorLexer
module Parser = OrchestratorParser
module Interpreter = OrchestratorInterpreter

let parse src =
  let module Wrapper = MakeParser (struct
    type ast = AST.stmt
    type token = Parser.token

    exception LexerError = Lexer.LexerError
    exception ParserError = Parser.Error

    let pp = AST_pp.pp_program
    let lexer = Lexer.read
    let parser = Parser.program
  end) in
  Wrapper.parse src

let type_check contract ast =
  TypeCheckOrchestrator.type_check_orchestrator contract ast

let symb_run (contract, program) ~mode =
  (* initialize each policy checker *)
  let policy_init_states =
    List.map PolicyChecker.init_policy contract.policies
  in
  OrchestratorInterpreter.build_symb_process program contract policy_init_states
  |> Symbolic.Runtime.Symex.run ~mode