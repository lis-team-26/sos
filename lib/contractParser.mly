%{
  open ContractAST    
%}

(* Tokens and precedence *)

%token COLON ARROW DOT ASSIGN EOF 
%token SUM AVG MIN MAX SORTED
%token LSQUARE RSQUARE LBRACE RBRACE
%token GLOBALS FUNCTIONS QOS POLICIES SERVICES
%token NAME PARAMS RETURNS PRECOND QOS_POSTCOND OK_POSTCOND ERR_POSTCOND EFFECTS CONSTRAINTS GROUPBY
%token INT_TYPE BOOL_TYPE

%start <contract> contract

%%

app_expr:
  | f = VAR ; LPAREN ; args = exprs(app_expr) ; RPAREN { EApp (f, args) }

contract_expr:
  | e = expr(app_expr) { e }

contract_exprs:
  | es = exprs(app_expr) { es }

delimited_comma_separeted_list(X):
  | LSQUARE ; l = separated_list(COMMA, X) ; RSQUARE { l }

contract:
  | LBRACE ;
      GLOBALS ; COLON ; globals = items ; COMMA
      FUNCTIONS ; COLON ; functions = functions ; COMMA
      QOS ; COLON ; qos = items ; COMMA
      POLICIES ; COLON ; policies = policies ; COMMA
      SERVICES ; COLON ; services = services ;  
    RBRACE ; EOF
  {
    { globals; functions; qos; policies; services }
  }

(* Generic items definition *)

items:
  | items = delimited_comma_separeted_list(item) { items }

item:
  | v = VAR ; COLON ; t = atom_type { (v, t) }

(* Functions and types *)

functions:
  | funcs = delimited_comma_separeted_list(func_item) { funcs }

func_item:
  | f = VAR ; COLON ; t = fun_type { (f, t) }

atom_type:
  | INT_TYPE { TInt }
  | BOOL_TYPE { TBool }

fun_type:
  | t = atom_type ; ARROW ; ret = atom_type { TFun ([ t ], ret) }
  | t = atom_type ; ARROW ; tt = fun_type { let TFun (ts, ret) = tt in TFun (t :: ts, ret) }

(* Policies *)

cmp_op:
  | LT { Lt }
  | LE { Le }
  | GT { Gt }
  | GE { Ge }
  | EQ { Eq }
  | NEQ { Neq }

aggr_op:
  | SUM { Sum }
  | AVG { Avg }
  | MIN { Min }
  | MAX { Max }

policies:
  | policies = delimited_comma_separeted_list(policy) { policies }

policy:
  | t = policy_type { (t, None) }
  | t = policy_type ; GROUPBY ; s = VAR { (t, Some s) }

policy_type:
  | r = regex { Regex r }
  | SORTED ; LPAREN ; v = VAR ; RPAREN { Sort v }
  | aggr_op = aggr_op ; LPAREN ; v = VAR ; RPAREN ; cmp_op = cmp_op ; i = INT { QosFieldOp (aggr_op, v, cmp_op, i) }

regex:
  | r = regex_choice { r }

regex_choice:
  | r1 = regex_choice ; PLUS ; r2 = regex_concat { RChoice (r1, r2) }
  | r = regex_concat { r }

regex_concat:
  | r1 = regex_concat ; DOT ; r2 = regex_atom { RConcat (r1, r2) }
  | r = regex_atom { r }

regex_atom:
  | s = VAR { RService s }
  | r = regex_atom ; TIMES { RStar r }
  | LPAREN ; e = regex ; RPAREN { e }

(* Services *)

services:
  | services = delimited_comma_separeted_list(service) { services }

service:
  | LBRACE ;
      NAME ; COLON ; name = VAR ; COMMA ;
      PARAMS ; COLON ; params = items ; COMMA ;
      RETURNS ; COLON ; LBRACE ; returns = item ; RBRACE ; COMMA ;
      PRECOND ; COLON ; precond = delimited_comma_separeted_list(contract_expr) ; COMMA ;
      QOS_POSTCOND ; COLON ; qos_postcond = postcond ; COMMA ;
      OK_POSTCOND ; COLON ; ok_postcond = postcond ; 
      err_postcond = option(COMMA ; ERR_POSTCOND ; COLON ; err_postcond = postcond { err_postcond }) ;
    RBRACE
  {
    { name; params; returns; precond; qos_postcond; ok_postcond; err_postcond }
  }

effects:    
  | EFFECTS ; COLON ; effects = delimited_comma_separeted_list(effct) { effects }

effct:
  | v = VAR ; ASSIGN ; e = contract_expr { (LVar v, e) }
  | v = VAR ; LPAREN ; args = contract_exprs ; RPAREN ; ASSIGN ; e = contract_expr { (LApp (v, args), e) }

constraints:
  | CONSTRAINTS ; COLON ; cs = delimited_comma_separeted_list(contract_expr) { cs }

postcond:
  | LBRACE es = effects ; COMMA ; cs = constraints ; RBRACE { (es, cs) }
