{
  open OrchestratorParser
  exception LexerError of string
}

let return = '\n' | "\r\n"
let white = [' ' '\t']+

let int = ['0'-'9']['0'-'9']*
let bool = "true" | "false"
let var = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let line_comment = "//" [^ '\n' '\r']*
let open_multiline_comment = "/*"
let close_multiline_comment = "*/"

rule read = parse
  (* Blank characters and comments *)
  | return { Lexing.new_line lexbuf; read lexbuf }
  | white { read lexbuf }
  | line_comment { read lexbuf }
  | open_multiline_comment { comment lexbuf; read lexbuf }
  (* Operators and symbols *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "<=" { LE }
  | "<" { LT }
  | ">=" { GE }
  | ">" { GT }
  | "==" { EQ }
  | "!=" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "!" { NOT }
  | "." { DOT }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | ":=" { ASSIGN }
  (* Keywords *)
  | "int?" { ANONDET }
  | "bool?" { BNONDET }
  | "skip" { SKIP }
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
  (* Delimiters *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  (* Literals and identifiers *)
  | int as n { INT (int_of_string n) }
  | bool as b { BOOL (b = "true") }
  | var as x { VAR x }
  | eof { EOF }
  | _ { raise (LexerError ("Unexpected character " ^ Lexing.lexeme lexbuf)) }

and comment = parse
  | close_multiline_comment { () }
  | return { Lexing.new_line lexbuf; comment lexbuf }
  | eof { raise (LexerError "Unterminated block comment") }
  | _ { comment lexbuf }