module AST = OrchestratorAST
module AST_pp = OrchestratorAST_pp
module TypedAST = TypedOrchestratorAST
module TypedAST_pp = TypedOrchestratorAST_pp
module Lexer = OrchestratorLexer
module Parser = OrchestratorParser
module TypeChecker = OrchestratorTypeChecker
module Interpreter = OrchestratorInterpreter

let parse src =
  let module Parser = Utils.Parser.Make (struct
    type ast = AST.stmt
    type token = Parser.token

    exception LexerError = Lexer.LexerError
    exception ParserError = Parser.Error

    let pp = AST_pp.pp_program
    let lexer = Lexer.read
    let parser = Parser.program
  end) in
  Parser.parse src

let type_check = TypeChecker.type_check_orchestrator

let run ~fuel contract program =
  Interpreter.build_symex_process ~fuel contract program
  |> Symbolic.Runtime.Symex.run_with_stats ~mode:Soteria.Symex.Approx.OX
