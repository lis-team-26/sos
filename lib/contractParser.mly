(*TODO: 
    - unify SLA and QoS per service
    - trust as keyword    
*)

%{
    open ContractAST    
%}

(* tokens *)
%token COLON ARROW DOT EQ COMMA EOF 
%token PLUS MINUS TIMES DIV LE LT GE GT NOT OR AND
%token SUM AVG MIN MAX SORTED
%token OPEN_PAR CLOSE_PAR OPEN_LIST CLOSE_LIST LBRACE RBRACE
%token GLOBALS FUNCTIONS QOS POLICIES SERVICES NAME PARAMS RETURNS TRUST PRECOND OK_POSTCOND ERR_POSTCOND 

%token <int> INT
%token <bool> BOOL
%token <string> VAR

%left OR
%left AND
%left PLUS
%left TIMES
%left DOT
%right NOT

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
arith_op:
    | PLUS  {Add}
    | MINUS {Sub}
    | TIMES {Mul}
    | DIV   {Div}

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
    | e=policy_bool                     {QosFieldOp(e)}
    | r=regex                           {Regex(r)}
    | SORTED OPEN_PAR id=VAR CLOSE_PAR  {Sort(id)}

policy_bool:
    | e1=policy_arith cmp=cmp_op e2=policy_arith    {PBinOp(cmp, e1, e2)}
    | NOT e=policy_bool                             {PUnOp(Not, e)}
    | OPEN_PAR e=policy_bool CLOSE_PAR              {e}

policy_arith:
    | n=INT                                         {PExpr(EInt n)}
    | v=VAR                                         {PExpr(EVar v)}
    | a=num_aggregate OPEN_PAR id=VAR CLOSE_PAR     {PAgg(a, id)}
    | e1=policy_arith aop=arith_op  e2=policy_arith {PBinOp(aop, e1, e2)}
    | OPEN_PAR e=policy_arith CLOSE_PAR             {e}



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
        PRECOND      COLON   pre=expr_list    COMMA
        QOS          COLON   qos=qos_constr   COMMA
        OK_POSTCOND  COLON   ok=expr_list     COMMA
        ERR_POSTCOND COLON   err=expr_list
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


(* ---------- QoS ---------- *)
qos_constr:
  | OPEN_LIST 
        es=separated_list(COMMA, expr)
    CLOSE_LIST
    { es }


(* ---------- EXPRESSIONS (prefix style) ---------- *)
expr_list:
    | OPEN_LIST es=exprs CLOSE_LIST { es }

exprs:
  | es=separated_list(COMMA, expr) { es }


    

atom:
    | n=INT                         {EInt(n)}
    | v=VAR                         {EVar(v)}
    | id=VAR OPEN_PAR args=exprs CLOSE_PAR          {EApp(id, args)}
    | OPEN_PAR e=expr CLOSE_PAR     {e}
    | b=BOOL                                        {EBool(b)}

expr:
    | a=atom                                {a}
    | e1=expr aop=arith_op e2=expr          {EBinOp(aop,e1,e2)}
    | e= expr DOT field=VAR                 {EField(e, field)}
    | e1=expr cmp=cmp_op e2=expr            {EBinOp(cmp,e1,e2)}    
    | e1=expr AND e2=expr                   {EBinOp(And,e1,e2)}
    | e1=expr OR e2=expr                    {EBinOp(Or,e1,e2)}
    | NOT e=expr                            {EUnOp(Not,e)}



typ:
    | VAR {
        match $1 with
        | "int" -> TInt
        | "bool" -> TBool
        | "Outcome" -> TOutcome
        | _ -> TCustom $1
        }