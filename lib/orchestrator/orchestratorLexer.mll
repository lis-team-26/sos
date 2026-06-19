{
  open OrchestratorParser
  open Utils.Data
  exception LexerError of string
}

let int = ['0'-'9']['0'-'9']*
let bool = "true" | "false"
let var = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let return = '\n' | "\r\n"
let white = [' ' '\t']+

rule read = parse
  | return { Lexing.new_line lexbuf; read lexbuf }
  | white { read lexbuf }
  | int as n { INT (int_of_string n) }
  | bool as b { BOOL (b = "true") }
  | "int?" { ANONDET }
  | "bool?" { BNONDET }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | "&&" { AND }
  | "||" { OR }
  | "!" { NOT }
  | "==" { EQ }
  | "!=" { NEQ }
  | '<' { LT }
  | "<=" { LE }
  | '>' { GT }
  | ">=" { GE }
  | "skip" { SKIP }
  | ":=" { ASSIGN }
  | ';' { SEMICOLON }
  | '.' { DOT }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "assume" { ASSUME }
  | "assert" { ASSERT (Loc { line = lexbuf.lex_curr_p.pos_lnum; col = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol + 1 }) }
  | "invoke" { INVOKE (Loc { line = lexbuf.lex_curr_p.pos_lnum; col = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol + 1 }) }
  | "int" { INT_TYPE }
  | "bool" { BOOL_TYPE }
  | "rcpt" { RECEIPT_TYPE }
  | "retval" { RETVAL }
  | "successful" { SUCCESSFUL }
  | "qos" { QOS }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | eof { EOF }
  | var as x { VAR x }
  | _ { raise (LexerError ("Unexpected character " ^ Lexing.lexeme lexbuf)) }
