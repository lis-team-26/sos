module AST = ContractAST
module AST_pp = ContractAST_pp
module TypedAST = TypedContractAST
module TypedAST_pp = TypedContractAST_pp
module Lexer = ContractLexer
module Parser = ContractParser
module TypeChecker = ContractTypeChecker

let parse src =
  let module Parser = Utils.Parser.Make (struct
    type ast = AST.contract
    type token = Parser.token

    exception LexerError = Lexer.LexerError
    exception ParserError = Parser.Error

    let pp = AST_pp.pp_contract
    let lexer = Lexer.read
    let parser = Parser.contract
  end) in
  Parser.parse src

let type_check = TypeChecker.type_check_contract
