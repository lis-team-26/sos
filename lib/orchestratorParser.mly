%{
  open OrchestratorAST
  open Utils.Types
%}

%token ANONDET BNONDET
%token SKIP ASSIGN SEMICOLON IF THEN ELSE WHILE DO
%token ASSUME ASSERT INVOKE
%token LBRACE RBRACE EOF
%token INT_TYPE BOOL_TYPE

%start <stmt> program

%%

program:
  | ss = stmts EOF { ss }

nondet_or_app_expr:
  | ANONDET { EIntNonDet }
  | BNONDET { EBoolNonDet }
  | f = VAR ; LPAREN ; args = exprs(nondet_or_app_expr) ; RPAREN { EApp (f, args) }

orchestrator_expr:
  | e = expr(nondet_or_app_expr) { e }

orchestrator_exprs:
  | es = exprs(nondet_or_app_expr) { es }

stmt:
  | s = atom_stmt { s }
  | s = structured_stmt { s }

stmts:
  | s = stmt ; ss = stmts { Seq (s, ss) }
  | s = stmt { s }

var_type:
  | INT_TYPE { TInt }
  | BOOL_TYPE { TBool }

atom_stmt:
  | SKIP ; SEMICOLON { Skip }
  | t = var_type ; x = VAR ; ASSIGN ; e = orchestrator_expr ; SEMICOLON { Declare (t, x, e) }
  | x = VAR ; ASSIGN ; e = orchestrator_expr ; SEMICOLON { Assign (x, e) }
  | ASSUME ; e = orchestrator_expr ; SEMICOLON { Assume e }
  | ASSERT ; e = orchestrator_expr ; SEMICOLON { Assert e }
  | INVOKE ; f = VAR ; LPAREN ; args = orchestrator_exprs ; RPAREN ; SEMICOLON
    { Invoke (f, args) }
  | t = var_type ; x = VAR ; ASSIGN ; INVOKE ; f = VAR ; LPAREN ; args = orchestrator_exprs ; RPAREN ; SEMICOLON
    { DeclareInvoke (t, x, f, args) }
  | x = VAR ; ASSIGN ; INVOKE ; f = VAR ; LPAREN ; args = orchestrator_exprs ; RPAREN ; SEMICOLON
    { AssignInvoke (x, f, args) }

structured_stmt:
  | IF ; e = orchestrator_expr ; THEN ; b1 = block ; ELSE ; b2 = block { If (e, b1, b2) }
  | IF ; e = orchestrator_expr ; THEN ; b = block { If (e, b, Skip) }
  | WHILE ; e = orchestrator_expr ; DO ; b = block { While (e, b) }

block:
  | s = atom_stmt { s }
  | LBRACE ; ss = stmts ; RBRACE { ss }
