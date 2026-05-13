module Ast = Ast
module Lexer = Lexer
module Parser = Parser
module SymbolicInterpreter = SymbolicInterpreter
module Utils = Utils

let parse src =
  let lexbuf = Lexing.from_channel (open_in src) in
  Parser.prog Lexer.read lexbuf

let symb_run (_, stmt) ~mode =
  stmt
  |> SymbolicInterpreter.build_symb_process
  |> SymbolicInterpreter.Symex.run ~mode
