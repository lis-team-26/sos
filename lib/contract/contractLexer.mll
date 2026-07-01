{
  open ContractParser
  exception LexerError of string
}

let return = '\n' | "\r\n"
let white = [' ' '\t']+

let int = ['0'-'9']['0'-'9']*
let bool = "true" | "false"
let char = '\'' ['a'-'z' 'A'-'Z' '0'-'9'] '\''
let regex = '"'(['a'-'z' 'A'-'Z' '0'-'9' '|' '(' ')' '[' ']' '*' '-' '^' '.' '?' '+']+)'"'
let var = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  (* Blank characters *)
  | return { Lexing.new_line lexbuf; read lexbuf }
  | white { read lexbuf }
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
  | ":=" { ASSIGN }
  | ":" { COLON }
  | "," { COMMA }
  | "->" { ARROW }
  (* Aggregate operators *)
  | "sum" { SUM }
  | "avg" { AVG }
  | "min" { MIN }
  | "max" { MAX }
  | "sorted" { SORTED }
  (* Delimiters *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LSQUARE }
  | "]" { RSQUARE }
  | "{" { LBRACE }
  | "}" { RBRACE }
  (* Keywords *)
  | "globals" { GLOBALS }
  | "globals_assumptions" { GLOBALS_ASSUMPTIONS }
  | "functions" { FUNCTIONS }  
  | "qos" | "QoS" { QOS }
  | "policies" { POLICIES }
  | "group-by" { GROUPBY }
  | "services" { SERVICES }  
  | "name" { NAME }
  | "params" { PARAMS }
  | "returns" { RETURNS }
  | "precond" { PRECOND }
  | ("qos" | "QoS") "-postcond" { QOS_POSTCOND }
  | "ok-postcond" { OK_POSTCOND }  
  | "err-postcond" { ERR_POSTCOND }
  | "effects" { EFFECTS }
  | "constraints" { CONSTRAINTS }
  | "int" { INT_TYPE }
  | "bool" { BOOL_TYPE }
  (* Literals and identifiers *)
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | bool { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
  | char { CHAR (String.get (Lexing.lexeme lexbuf) 1) }
  | regex { REG (let s = Lexing.lexeme lexbuf in String.sub s 1 ((String.length s) - 2)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ as c
    { raise (LexerError
      (Fmt.str "Unexpected character '%c' at position %d" c (Lexing.lexeme_start lexbuf))) }
