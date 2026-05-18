module AST = ContractAST
module AST_pp = ContractAST_pp
module Lexer = ContractLexer
module Parser = ContractParser

let parse src =
  let module Wrapper = Utils.MakeParser (struct
    type ast = AST.contract
    type token = Parser.token

    exception LexerError = Lexer.LexerError
    exception ParserError = Parser.Error

    let pp = AST_pp.pp_contract
    let lexer = Lexer.read
    let parser = Parser.contract
  end) in
  Wrapper.parse src
