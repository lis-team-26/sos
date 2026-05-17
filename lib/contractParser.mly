(*TODO: 
    - unify SLA and QoS per service
    - trust as keyword    
*)

%{
    open ContractAST    
%}

(* tokens *)
%token COLON ARROW DOT EQ ASSIGN COMMA EOF 
%token PLUS MINUS TIMES DIV LE LT GE GT NOT OR AND
%token SUM AVG MIN MAX SORTED
%token OPEN_PAR CLOSE_PAR OPEN_LIST CLOSE_LIST LBRACE RBRACE
%token GLOBALS FUNCTIONS QOS POLICIES SERVICES NAME PARAMS RETURNS TRUST PRECOND OK_POSTCOND ERR_POSTCOND EFFECTS CONSTRAINTS 
%token INT_TYPE BOOL_TYPE

%token <int> INT
%token <bool> BOOL
%token <string> VAR

(*%left LT LE GT GE EQ*)
%left OR
%left AND
%left PLUS MINUS
%left TIMES DIV
%left DOT
%right NOT
%left DOT

%start <ContractAST.program> prg

%%

prg:
    | LBRACE 
        GLOBALS   COLON  g=globals     COMMA
        FUNCTIONS COLON  f=functions   COMMA
        QOS       COLON  qos=qos_def   COMMA
        POLICIES  COLON  p=policies    COMMA
        SERVICES  COLON  s=services  
      RBRACE EOF

    {
        {globals = g; functions = f; qos = qos; policies = p; services = s}
    }

(* ---------- GLOBALS ---------- *)
globals:
    | OPEN_LIST gs=separated_list(COMMA, global_item) CLOSE_LIST   {gs}

global_item:
    |  id=VAR COLON t=typ  {(id,t)}


(* ---------- FUNCTIONS ---------- *)
functions:
  | OPEN_LIST fs=separated_list(COMMA, func_item) CLOSE_LIST   {fs}

func_item:
    | id=VAR COLON ft=fun_type      { {fname=id; ty=ft} }

fun_type:
    | t1=typ ARROW t2=fun_type      {TArrow(t1, t2)}
    | t=typ                         {TBase(t)}

(* ---------- QOS Def---------- *)
qos_def:
    | OPEN_LIST qs=separated_list(COMMA, qos_decl) CLOSE_LIST {qs}
    
qos_decl:
    | id=VAR COLON t=typ    {(id, t)}



(* ---------- Operators ---------- *)
cmp_op:
    | LT {Lt}
    | LE {Le}
    | GT {Gt}
    | GE {Ge}
    | EQ {Eq}

num_aggregate: 
    | SUM       {Sum}
    | AVG       {Avg}
    | MIN       {Min}
    | MAX       {Max}

(* ---------- Policies ---------- *)
policies:
    | OPEN_LIST ps=separated_list(COMMA, policy) CLOSE_LIST {ps}

policy:
    | n=num_aggregate OPEN_PAR id=VAR CLOSE_PAR cmp=cmp_op i=INT {QosFieldOp(cmp, n, id, i)}
    | r=regex                           {Regex(r)}
    | SORTED OPEN_PAR id=VAR CLOSE_PAR  {Sort(id)}

regex:
    | OPEN_PAR e=regex CLOSE_PAR            {e}
    | s=VAR                                 {RService(s)}
    | r1=regex PLUS r2=regex                {RChoice(r1, r2)}
    | r1=regex DOT r2=regex                 {RConcat(r1, r2)}
    | r=regex TIMES                         {RStar(r)}


(* ---------- SERVICES ---------- *)
services:
    | OPEN_LIST ss=separated_list(COMMA, service) CLOSE_LIST { ss }


service:
    | LBRACE
        NAME         COLON   n=VAR            COMMA
        PARAMS       COLON   ps=params        COMMA
        RETURNS      COLON   rs=returns       COMMA
        TRUST        COLON   tr=INT           COMMA
        PRECOND      COLON   pre=bexpr_list   COMMA
        QOS          COLON   qos=behavior     COMMA
        OK_POSTCOND  COLON   ok=behavior      COMMA
        ERR_POSTCOND COLON   err=behavior
      RBRACE
    
    {
      {
        name = n;
        params = ps;
        returns = rs;
        trust = tr;
        precond = pre;
        qos = qos;
        ok_post = ok;
        err_post = err;
      }
    }

params:
    | OPEN_LIST p=separated_list(COMMA, param) CLOSE_LIST { p }

param:
  | id=VAR COLON t=typ { (id, t) }



returns:
    | OPEN_LIST r=separated_list(COMMA, return_item) CLOSE_LIST { r }

return_item:
    | id=VAR COLON t=typ { (id, t) }


effects:    
    | EFFECTS COLON OPEN_LIST e=separated_list(COMMA, effct) CLOSE_LIST { e }

effct:
    | id=VAR ASSIGN e=arith_expr                                                            {(LVar(id), e)}
    | id=VAR OPEN_PAR args=aexprs CLOSE_PAR ASSIGN e=arith_expr  {(LApp(id, args), e)}

constraints:
    | CONSTRAINTS COLON OPEN_LIST c=separated_list(COMMA, constrnt) CLOSE_LIST { c }

constrnt:
    | id=VAR cmp=cmp_op e=arith_expr                                    {(cmp, LVar(id), e)}
    | id=VAR OPEN_PAR args=aexprs CLOSE_PAR cmp=cmp_op e=arith_expr      {(cmp, LApp(id, args), e)}
    | id=VAR OPEN_PAR args=aexprs CLOSE_PAR                              {(Eq, LApp(id, args), EBool(true))} 

behavior:
    | LBRACE eff=effects COMMA constr=constraints RBRACE {(eff, constr)}



(* ---------- EXPRESSIONS (prefix style) ---------- *)
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



typ:
    | INT_TYPE     {TInt}
    | BOOL_TYPE    {TBool}



(* ---------- Expr v2 ---------- *)
atom:
    | v=VAR                                         {EVar(v)}
    | id=VAR OPEN_PAR args=aexprs CLOSE_PAR         {EApp(id, args)}
    | n=INT                                         {EInt(n)}


(*
arith_atom:
    | a=atom                                        {a}
    | n=INT                                         {EInt(n)}
*)


arith_expr:
    | a=atom                                  {a}
    | e1=arith_expr PLUS e2=arith_expr              {EBinOp(Add,e1,e2)}
    | e1=arith_expr MINUS e2=arith_expr             {EBinOp(Sub,e1,e2)}
    | e1=arith_expr TIMES e2=arith_expr             {EBinOp(Mul,e1,e2)}
    | e1=arith_expr DIV e2=arith_expr               {EBinOp(Div,e1,e2)}
    | OPEN_PAR e=arith_expr CLOSE_PAR               {e}

aexprs:
    |  es=separated_list(COMMA, arith_expr)  { es }


bool_expr:
    | b=BOOL                                        {EBool(b)}
    | id=VAR OPEN_PAR args=aexprs CLOSE_PAR         {EApp(id, args)}
    | e1=arith_expr cmp=cmp_op e2=arith_expr        {EBinOp(cmp,e1,e2)}    
    | e1=bool_expr AND e2=bool_expr                 {EBinOp(And,e1,e2)}
    | e1=bool_expr OR e2=bool_expr                  {EBinOp(Or,e1,e2)}
    | NOT e=bool_expr                               {EUnOp(Not,e)}
    | OPEN_PAR e=bool_expr CLOSE_PAR                {e}

bexpr_list:
    | OPEN_LIST es=separated_list(COMMA, bool_expr) CLOSE_LIST {es}