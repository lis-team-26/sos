open Contract.TypedAST
open Symbolic.Runtime
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

let run ~steps_fuel ~branching_fuel ~unroll_fuel contract program =
  let fuel =
    { steps = steps_fuel; branching = branching_fuel; unroll = unroll_fuel }
  in
  OrchestratorInterpreter.build_symex_process program contract fuel
  |> Symbolic.Runtime.Symex.run_with_stats ~mode:Soteria.Symex.Approx.OX
