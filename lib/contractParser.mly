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

%token COLON ARROW DOT EQ ASSIGN COMMA EOF 
%token PLUS MINUS TIMES DIV LE LT GE GT NOT OR AND
%token SUM AVG MIN MAX SORTED
%token OPEN_PAR CLOSE_PAR OPEN_LIST CLOSE_LIST LBRACE RBRACE
%token GLOBALS FUNCTIONS QOS POLICIES SERVICES NAME PARAMS RETURNS TRUST PRECOND OK_POSTCOND ERR_POSTCOND EFFECTS CONSTRAINTS 
%token INT_TYPE BOOL_TYPE

(*%left LT LE GT GE EQ*)
%left OR
%left AND
%left PLUS MINUS
%left TIMES DIV
%left DOT
%right NOT

%start <contract> contract

%%

delimited_comma_separeted_list(X):
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
  | id = VAR ; OPEN_PAR ; args = arith_exprs ; CLOSE_PAR { EApp (id, args) }
  | e1 = arith_expr ; op = PLUS ; e2 = arith_expr { EBinOp (Add, e1, e2) }
  | e1 = arith_expr ; op = MINUS ; e2 = arith_expr { EBinOp (Sub, e1, e2) }
  | e1 = arith_expr ; op = TIMES ; e2 = arith_expr { EBinOp (Mul, e1, e2) }
  | e1 = arith_expr ; op = DIV ; e2 = arith_expr { EBinOp (Div, e1, e2) }
  | OPEN_PAR ; e = arith_expr ; CLOSE_PAR { e }

arith_exprs:
  | es = separated_list(COMMA, arith_expr) { es }

bool_expr:
  | b = BOOL { EBool b }
  (* | id = VAR ; OPEN_PAR ; args = arith_exprs ; CLOSE_PAR { EApp (id, args) } *)
  | e1 = arith_expr ; cmp = cmp_op ; e2 = arith_expr { EBinOp (cmp, e1, e2) }
  | e1 = bool_expr ; AND ; e2 = bool_expr { EBinOp (And, e1, e2) }
  | e1 = bool_expr ; OR ; e2 = bool_expr { EBinOp (Or, e1, e2) }
  | NOT ; e = bool_expr { EUnOp (Not, e) }
  | OPEN_PAR ; e = bool_expr ; CLOSE_PAR { e }

(* Functions and types *)

functions:
  | funcs = delimited_comma_separeted_list(func_item) { funcs }

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
  | items = delimited_comma_separeted_list(item_definition) { items }

item_definition:
  | id = VAR ; COLON ; t = typ { (id, t) }

(* Policies *)

policies:
  | policies = delimited_comma_separeted_list(policy) { policies }

policy:
  | r = regex { Regex r }
  | SORTED ; OPEN_PAR id = VAR CLOSE_PAR { Sort id }
  | n = aggr_op ; OPEN_PAR id = VAR CLOSE_PAR ; cmp = cmp_op ; i = INT { QosFieldOp (cmp, n, id, i) }

regex:
  | OPEN_PAR ; e = regex ; CLOSE_PAR { e }
  | s = VAR { RService s}
  | r1 = regex ; PLUS ; r2 = regex { RChoice (r1, r2) }
  | r1 = regex ; DOT ; r2 = regex { RConcat (r1, r2) }
  | r = regex ; TIMES { RStar r }

(* Services *)

services:
  | services = delimited_comma_separeted_list(service) { services }

service:
  | LBRACE ;
      NAME ; COLON ; name = VAR ; COMMA ;
      PARAMS ; COLON ; params = items_definition ; COMMA ;
      RETURNS ; COLON ; returns = items_definition ; COMMA ;
      TRUST ; COLON ; trust = INT ; COMMA ;
      PRECOND ; COLON ; precond = delimited_comma_separeted_list(bool_expr) ; COMMA ;
      QOS ; COLON ; qos = behavior ; COMMA ;
      OK_POSTCOND ; COLON ; ok_post = behavior ; COMMA ;
      ERR_POSTCOND ; COLON ; err_post = behavior ;
    RBRACE
  {
    { name; params; returns; trust; precond; qos; ok_post; err_post }
  }

effects:    
  | EFFECTS ; COLON ; effects = delimited_comma_separeted_list(effct) { effects }

effct:
  | id = VAR ; ASSIGN ; e = arith_expr { (LVar id, e) }
  | id = VAR ; OPEN_PAR ; args = arith_exprs ; CLOSE_PAR ; ASSIGN ; e = arith_expr { (LApp (id, args), e) }

constraints:
  | CONSTRAINTS ; COLON ; constraints = delimited_comma_separeted_list(constrnt) { constraints }

constrnt:
  | id = VAR ; cmp = cmp_op ; e = arith_expr { (cmp, LVar id, e) }
  | id = VAR ; OPEN_PAR ; args = arith_exprs ; CLOSE_PAR ; cmp = cmp_op ; e = arith_expr { (cmp, LApp (id, args), e) }
  | id = VAR ; OPEN_PAR ; args = arith_exprs ; CLOSE_PAR { (Eq, LApp (id, args), EBool true)}  

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
  | id=VAR OPEN_PAR args=exprs CLOSE_PAR          {EApp(id, args)}
  | OPEN_PAR e=expr CLOSE_PAR                     {e}
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