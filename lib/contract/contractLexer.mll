{
  open ContractParser
  exception LexerError of string
}

(* regexp *)
let whitespace = [' ' '\t']+ | '\r' | '\n' | "\r\n"
let integer = '-'?['0' - '9']['0' - '9']*
let bool = "true" | "false"
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let regex = '"'(['a'-'z' 'A'-'Z' '0'-'9' '|' '(' ')' '[' ']' '*' '-' '^' '.' '?' '+']+)'"'

rule read = parse
  (* Symbols and operators *)
  | ":" { COLON }
  | "," { COMMA }
  | "->" { ARROW }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "<=" { LE }
  | "<" { LT }
  | ">=" { GE }
  | ">" { GT }
  | "!" { NOT }
  | "&&" { AND }
  | "||" { OR }
  | "=" { EQ }
  | ":=" { ASSIGN }
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
  | whitespace { read lexbuf }
  | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | bool { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
  | id { VAR (Lexing.lexeme lexbuf) }
  | regex { REG (let s = Lexing.lexeme lexbuf in String.sub s 1 ((String.length s) - 2)) }
  | eof { EOF }
  | _ as c {
      raise (LexerError (Printf.sprintf "Unexpected character '%c' at position %d" c (Lexing.lexeme_start lexbuf)))
    }
