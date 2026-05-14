{
    open ContractParser
    exception LexingError of string
}

(* regexp *)
let whitespace = [' ' '\t']+ | '\r' | '\n' | "\r\n"
let integer = '-'?['0' - '9']['0' - '9']*
let bool = "true" | "false"
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
| ":"       {COLON}
| ","       {COMMA}
| "."       {DOT}
| "->"      {ARROW}
| "+"       {PLUS}
| "-"       {MINUS}
| "*"       {TIMES}
| "/"       {DIV}
| "<="      {LE}
| "<"       {LT}
| ">="      {GE}
| ">"       {GT}
| "!"       {NOT}
| "&&"      {AND}
| "||"      {OR}
| "="       {EQ}


(* Aggregate operators *)
| "sum"     {SUM}
| "avg"     {AVG}
| "min"     {MIN}
| "max"     {MAX}
| "sorted"  {SORTED}

| "("       {OPEN_PAR}
| ")"       {CLOSE_PAR}
| "["       {OPEN_LIST}
| "]"       {CLOSE_LIST}
| "{"       {LBRACE}
| "}"       {RBRACE}

(* KEYWORDS *)
| "globals"         {GLOBALS}
| "functions"       {FUNCTIONS}  
| "QoS"             {QOS}
| "policies"        {POLICIES}
| "services"        {SERVICES}  
| "name"            {NAME}
| "params"          {PARAMS}
| "returns"         {RETURNS}
| "trusted"         {TRUST}
| "precond"         {PRECOND}
| "ok-postcond"     {OK_POSTCOND}  
| "err-postcond"    {ERR_POSTCOND}

| whitespace {read lexbuf}
| integer {INT (int_of_string (Lexing.lexeme lexbuf))}
| bool {BOOL (bool_of_string (Lexing.lexeme lexbuf))}
| id {VAR (Lexing.lexeme lexbuf)}
| eof {EOF}

| _ as c { raise (LexingError (Printf.sprintf "Unexpected character '%c' at position %d" c (Lexing.lexeme_start lexbuf))) }
