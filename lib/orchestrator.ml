module AST = OrchestratorAST
module AST_pp = OrchestratorAST_pp
module Lexer = OrchestratorLexer
module Parser = OrchestratorParser
module Interpreter = SymbolicInterpreter

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
  (* initialize each policy checker *)
  let policy_init_states =
    List.map PolicyChecker.init_policy contract.policies
  in
  SymbolicInterpreter.build_symb_process program contract policy_init_states
  |> SymbolicInterpreter.Symex.run ~mode
