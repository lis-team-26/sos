%{
  open ContractAST
  open Utils.Loc
  open Utils.Types
%}

%token <string> REG
%token <char> CHAR

%token COLON ARROW ASSIGN EOF 
%token SUM AVG MIN MAX SORTED
%token LSQUARE RSQUARE LBRACE RBRACE
%token GLOBALS GLOBALS_ASSUMPTIONS FUNCTIONS QOS POLICIES SERVICES
%token NAME PARAMS RETURNS PRECOND QOS_POSTCOND OK_POSTCOND ERR_POSTCOND EFFECTS CONSTRAINTS GROUPBY
%token INT_TYPE BOOL_TYPE

%start <contract> contract

%%

(* Generic list *)

delimited_comma_separated_list(X):
  | LSQUARE ; l = separated_list(COMMA, X) ; RSQUARE { l }

(* Expressions *)

app_expr:
  | f = VAR ; LPAREN ; args = exprs(app_expr) ; RPAREN
    { EApp (f, args) |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }

contract_expr:
  | e = expr(app_expr) { e }

contract_exprs:
  | es = exprs(app_expr) { es }

(* Types, typed identificators and functions *)

atom_type:
  | INT_TYPE { TInt }
  | BOOL_TYPE { TBool }

fun_type:
  | t = atom_type ; ARROW ; ret = atom_type { TFun ([ t ], ret) }
  | t = atom_type ; ARROW ; tt = fun_type { let TFun (ts, ret) = tt in TFun (t :: ts, ret) }

typed_ident:
  | v = VAR ; COLON ; t = atom_type { (v, t) }

located_typed_ident:
  | i = typed_ident { i |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }

located_typed_fun:
  | f = VAR ; COLON ; t = fun_type
    { (f, t) |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }

located_typed_idents:
  | is = delimited_comma_separated_list(located_typed_ident) { is }

located_typed_funcs:
  | fs = delimited_comma_separated_list(located_typed_fun) { fs }

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

policy:
  | t = policy_type
    { (t, None) |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | t = policy_type ; GROUPBY ; s = VAR
    { (t, Some s) |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }

policies:
  | policies = delimited_comma_separated_list(policy) { policies }

service_char:
  | s = VAR ; ARROW ; c = CHAR { (s, c) }

policy_type:
  | LBRACE ; m = separated_list(COMMA, service_char) ; RBRACE ; r = REG { Regex (m,r) }
  | SORTED ; LPAREN ; v = VAR ; RPAREN { Sort v }
  | aggr_op = aggr_op ; LPAREN ; v = VAR ; RPAREN ; cmp_op = cmp_op ; i = INT { QosFieldOp (aggr_op, v, cmp_op, i) }

(* Services *)

effects:    
  | EFFECTS ; COLON ; effects = delimited_comma_separated_list(effct) { effects }

effect_lhs:
  | v = VAR { LVar v |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | f = VAR ; LPAREN ; args = contract_exprs ; RPAREN
    { LApp (f, args) |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }

effct:
  | lhs = effect_lhs ; ASSIGN ; e = contract_expr { (lhs, e) }

constraints:
  | CONSTRAINTS ; COLON ; cs = delimited_comma_separated_list(contract_expr) { cs }

postcond:
  | LBRACE es = effects ; COMMA ; cs = constraints ; RBRACE { (es, cs) }

service:
  | LBRACE ;
      NAME ; COLON ; name = VAR ; COMMA ;
      PARAMS ; COLON ; params = located_typed_idents ; COMMA ;
      RETURNS ; COLON ; LBRACE ; returns = typed_ident ; RBRACE ; COMMA ;
      PRECOND ; COLON ; precond = delimited_comma_separated_list(contract_expr) ; COMMA ;
      QOS_POSTCOND ; COLON ; qos_postcond = postcond ; COMMA ;
      OK_POSTCOND ; COLON ; ok_postcond = postcond ; 
      err_postcond = option(COMMA ; ERR_POSTCOND ; COLON ; err_postcond = postcond { err_postcond }) ;
    RBRACE
    {
      { name; params; returns; precond; qos_postcond; ok_postcond; err_postcond }
        |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos
    }

services:
  | services = delimited_comma_separated_list(service) { services }

contract:
  | LBRACE ;
      GLOBALS ; COLON ; globals = located_typed_idents ; COMMA
      GLOBALS_ASSUMPTIONS ; COLON ; globals_assumptions = delimited_comma_separated_list(contract_expr) ; COMMA
      FUNCTIONS ; COLON ; functions = located_typed_funcs ; COMMA
      QOS ; COLON ; qos = located_typed_idents ; COMMA
      tail = contract_tail ;
    RBRACE ; EOF
    {
      let services, policies = tail in
        { globals; globals_assumptions; functions; qos; policies; services }
    }

contract_tail:
  | SERVICES ; COLON ; services = services ; COMMA
    POLICIES ; COLON ; policies = policies
      { (services, policies) }
  | POLICIES ; COLON ; policies = policies ; COMMA
    SERVICES ; COLON ; services = services
      { (services, policies) }