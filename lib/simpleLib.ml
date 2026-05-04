module AST = SimpleAST
module Lexer = SimpleLexer
module Parser = SimpleParser
module SymbInterpreter = SimpleSymbInterpreter
module Utils = SimpleUtils

let parse src =
  let lexbuf = Lexing.from_channel (open_in src) in
  Parser.prog Lexer.read lexbuf

let symb_run (_, stmt) ~mode =
  stmt
  |> SymbInterpreter.build_symb_process
  |> SymbInterpreter.Symex.run ~mode
