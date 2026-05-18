{
  open OrchestratorParser
  exception LexerError of string
}

let int = ['0'-'9']['0'-'9']*
let bool = "true" | "false"
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let white = [' ' '\t' '\n' '\r']+ | "\r\n"

rule read = parse
  | white { read lexbuf }
  | int as n { INT (int_of_string n) }
  | bool as b { BOOL (b = "true") }
  | '?' { NONDET }
  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
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
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "assume" { ASSUME }
  | "assert" { ASSERT }
  | "invoke" { INVOKE }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | eof { EOF }
  | id as x { ID x }
  | _ { raise (LexerError ("Unexpected character " ^ Lexing.lexeme lexbuf)) }
