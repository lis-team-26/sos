(*
TODO: 
  - unify SLA and QoS per service
  - trust as keyword    
  - fix conflict between arithmetic and boolean function calls
    (either allow only arithmetic function call or unify both NT)
*)

%{
  open ContractAST    
%}

(* Tokens and precedence *)

%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token <string> REG

%token COLON ARROW EQ ASSIGN COMMA EOF 
%token PLUS MINUS TIMES DIV LE LT GE GT NOT OR AND
%token SUM AVG MIN MAX SORTED
%token LPAREN RPAREN OPEN_LIST CLOSE_LIST LBRACE RBRACE
%token GLOBALS FUNCTIONS QOS POLICIES SERVICES NAME PARAMS RETURNS TRUST PRECOND OK_POSTCOND ERR_POSTCOND EFFECTS CONSTRAINTS GROUPBY
%token INT_TYPE BOOL_TYPE

(*%left LT LE GT GE EQ*)
%left OR
%left AND
%left PLUS MINUS
%left TIMES DIV
%right NOT

%start <contract> contract

%%

delimited_comma_separated_list(X):
  | OPEN_LIST ; l = separated_list(COMMA, X) ; CLOSE_LIST { l }

contract:
  | LBRACE ;
      GLOBALS ; COLON ;  globals = items_definition ; COMMA
      FUNCTIONS ; COLON ;  functions = functions ; COMMA
      QOS ; COLON ;  qos = items_definition ; COMMA
      POLICIES ; COLON ;  policies = policies ; COMMA
      SERVICES ; COLON ;  services = services ;  
    RBRACE ; EOF
  {
    { globals; functions; qos; policies; services }
  }

(* Arithmetic and boolean expressions *)

(*
arith_atom:
  | a = atom { a }
  | n = INT { EInt n }
*)

arith_atom:
  | v = VAR { EVar v }
  | n = INT { EInt n }

cmp_op:
  | LT { Lt }
  | LE { Le }
  | GT { Gt }
  | GE { Ge }
  | EQ { Eq }

aggr_op: 
  | SUM { Sum }
  | AVG { Avg }
  | MIN { Min }
  | MAX { Max }

arith_expr:
  | a = arith_atom { a }
  | id = VAR ; LPAREN ; args = arith_exprs ; RPAREN { EApp (id, args) }
  | e1 = arith_expr ; op = PLUS ; e2 = arith_expr { EBinOp (Add, e1, e2) }
  | e1 = arith_expr ; op = MINUS ; e2 = arith_expr { EBinOp (Sub, e1, e2) }
  | e1 = arith_expr ; op = TIMES ; e2 = arith_expr { EBinOp (Mul, e1, e2) }
  | e1 = arith_expr ; op = DIV ; e2 = arith_expr { EBinOp (Div, e1, e2) }
  | LPAREN ; e = arith_expr ; RPAREN { e }

arith_exprs:
  | es = separated_list(COMMA, arith_expr) { es }

bool_expr:
  | b = BOOL { EBool b }
  (* | id = VAR ; LPAREN ; args = arith_exprs ; RPAREN { EApp (id, args) } *)
  | e1 = arith_expr ; cmp = cmp_op ; e2 = arith_expr { EBinOp (cmp, e1, e2) }
  | e1 = bool_expr ; AND ; e2 = bool_expr { EBinOp (And, e1, e2) }
  | e1 = bool_expr ; OR ; e2 = bool_expr { EBinOp (Or, e1, e2) }
  | NOT ; e = bool_expr { EUnOp (Not, e) }
  | LPAREN ; e = bool_expr ; RPAREN { e }

(* Functions and types *)

functions:
  | funcs = delimited_comma_separated_list(func_item) { funcs }

func_item:
  | fname = VAR ; COLON ; ty = fun_type { { fname; ty } }

typ:
  | INT_TYPE { TInt }
  | BOOL_TYPE { TBool }

fun_type:
  | t = typ { TBase t }
  | t1 = typ ; ARROW ; t2 = fun_type { TArrow (t1, t2) }

(* Generic items definition *)

items_definition:
  | items = delimited_comma_separated_list(item_definition) { items }

item_definition:
  | id = VAR ; COLON ; t = typ { (id, t) }

(* Policies *)

policies:
  | policies = delimited_comma_separated_list(policy) { policies }

policy:
  | t = policy_type { (t, None) }
  | t = policy_type GROUPBY s = VAR { (t, Some s) }

service_char:
  | s = VAR ; ARROW ; c = VAR {(s, (String.get c 0))}

policy_type:
  | LBRACE ; m = separated_list(COMMA, service_char) ; RBRACE ; r = REG { Regex (m,r) }
  | SORTED ; LPAREN ; v = VAR ; RPAREN { Sort v }
  | aggr_op = aggr_op ; LPAREN ; v = VAR ; RPAREN ; cmp_op = cmp_op ; i = INT { QosFieldOp (aggr_op, v, cmp_op, i) }


(* Services *)

services:
  | services = delimited_comma_separated_list(service) { services }

service:
  | LBRACE ;
      NAME ; COLON ; name = VAR ; COMMA ;
      PARAMS ; COLON ; params = items_definition ; COMMA ;
      RETURNS ; COLON ; returns = items_definition ; COMMA ;
      TRUST ; COLON ; trust = INT ; COMMA ;
      PRECOND ; COLON ; precond = delimited_comma_separated_list(bool_expr) ; COMMA ;
      QOS ; COLON ; qos = behavior ; COMMA ;
      OK_POSTCOND ; COLON ; ok_post = behavior ; COMMA ;
      ERR_POSTCOND ; COLON ; err_post = behavior ;
    RBRACE
  {
    { name; params; returns; trust; precond; qos; ok_post; err_post }
  }

effects:    
  | EFFECTS ; COLON ; effects = delimited_comma_separated_list(effct) { effects }

effct:
  | id = VAR ; ASSIGN ; e = arith_expr { (LVar id, e) }
  | id = VAR ; LPAREN ; args = arith_exprs ; RPAREN ; ASSIGN ; e = arith_expr { (LApp (id, args), e) }

constraints:
  | CONSTRAINTS ; COLON ; constraints = delimited_comma_separated_list(constrnt) { constraints }

constrnt:
  | id = VAR ; cmp = cmp_op ; e = arith_expr { (cmp, LVar id, e) }
  | id = VAR ; LPAREN ; args = arith_exprs ; RPAREN ; cmp = cmp_op ; e = arith_expr { (cmp, LApp (id, args), e) }
  | id = VAR ; LPAREN ; args = arith_exprs ; RPAREN { (Eq, LApp (id, args), EBool true)}  

behavior:
  | LBRACE eff = effects ; COMMA ; constr = constraints ; RBRACE { (eff, constr) }

(* Old expressions style *)
(*
expr_list:
  | OPEN_LIST es=exprs CLOSE_LIST { es }

exprs:
  | es=separated_list(COMMA, expr) { es }

atom:
  | n=INT                                         {EInt(n)}
  | v=VAR                                         {EVar(v)}
  | id=VAR LPAREN args=exprs RPAREN          {EApp(id, args)}
  | LPAREN e=expr RPAREN                     {e}
  | b=BOOL                                        {EBool(b)}
  | id=VAR DOT field=VAR                          {EField(EVar(id), field)}

expr:
  | a=atom                                        {a}
  | e1=expr aop=arith_op e2=expr                  {EBinOp(aop,e1,e2)}
  | e1=expr cmp=cmp_op e2=expr                    {EBinOp(cmp,e1,e2)}    
  | e1=expr AND e2=expr                           {EBinOp(And,e1,e2)}
  | e1=expr OR e2=expr                            {EBinOp(Or,e1,e2)}
  | NOT e=expr                                    {EUnOp(Not,e)}
*)
