{
  open OrchestratorParser
  exception LexerError of string
}

let int = ['0'-'9']['0'-'9']*
let bool = "true" | "false"
let var = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let return = '\n' | "\r\n"
let white = [' ' '\t']+
let line_comment = "//" [^ '\n' '\r']*
let open_multiline_comment = "/*"
let close_multiline_comment = "*/"

rule read = parse
  | return { Lexing.new_line lexbuf; read lexbuf }
  | white { read lexbuf }
  | int as n { INT (int_of_string n) }
  | bool as b { BOOL (b = "true") }
  | line_comment { read lexbuf }
  | open_multiline_comment { comment lexbuf; read lexbuf }
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
  | "assert" { ASSERT }
  | "invoke" { INVOKE }
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

and comment = parse
  | close_multiline_comment { () }
  | return { Lexing.new_line lexbuf; comment lexbuf }
  | eof { raise (LexerError "Unterminated block comment") }
  | _ { comment lexbuf }