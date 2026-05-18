%{
  open OrchestratorAST
%}

%token <int> INT
%token <string> ID
%token <bool> BOOL
%token NONDET
%token ADD SUB MUL DIV
%token AND OR NOT
%token EQ NEQ LT LE GT GE
%token SKIP ASSIGN SEMICOLON IF THEN ELSE WHILE DO
%token ASSUME ASSERT INVOKE
%token COMMA LPAREN RPAREN LBRACE RBRACE EOF

%start <stmt> program

%left OR
%left AND
%nonassoc NOT
%left ADD SUB
%left MUL DIV

%%

sequence(X):
  | x = X ; y = sequence(X) { Seq (x, y) }
  | x = X { x }

program:
  | s = sequence(stmt) EOF { s }

aexpr:
  | n = INT { Int n }
  | x = ID { Var x }
  | NONDET { NonDet }
  | aexpr1 = aexpr ; ADD ; aexpr2 = aexpr { AOp (aexpr1, Add, aexpr2) }
  | aexpr1 = aexpr ; SUB ; aexpr2 = aexpr { AOp (aexpr1, Sub, aexpr2) }
  | aexpr1 = aexpr ; MUL ; aexpr2 = aexpr { AOp (aexpr1, Mul, aexpr2) }
  | aexpr1 = aexpr ; DIV ; aexpr2 = aexpr { AOp (aexpr1, Div, aexpr2) }
  | LPAREN ; aexpr = aexpr ; RPAREN { aexpr }

bexpr:
  | b = BOOL { Bool b }
  | NOT ; bexpr = bexpr { Not bexpr }
  | bexpr1 = bexpr ; AND ; bexpr2 = bexpr { BOp (bexpr1, And, bexpr2) }
  | bexpr1 = bexpr ; OR ; bexpr2 = bexpr { BOp (bexpr1, Or, bexpr2) }
  | aexpr1 = aexpr ; EQ ; aexpr2 = aexpr { COp (aexpr1, Eq, aexpr2) }
  | aexpr1 = aexpr ; NEQ ; aexpr2 = aexpr { COp (aexpr1, Neq, aexpr2) }
  | aexpr1 = aexpr ; LT ; aexpr2 = aexpr { COp (aexpr1, Lt, aexpr2) }
  | aexpr1 = aexpr ; LE ; aexpr2 = aexpr { COp (aexpr1, Le, aexpr2) }
  | aexpr1 = aexpr ; GT ; aexpr2 = aexpr { COp (aexpr1, Gt, aexpr2) }
  | aexpr1 = aexpr ; GE ; aexpr2 = aexpr { COp (aexpr1, Ge, aexpr2) }
  | LPAREN ; bexpr = bexpr ; RPAREN { bexpr }

stmt:
  | atom_stmt = atom_stmt { atom_stmt }
  | structured_stmt = structured_stmt { structured_stmt }

block:
  | atom_stmt = atom_stmt { atom_stmt }
  | LBRACE ; stmt = sequence(stmt) ; RBRACE { stmt }

atom_stmt:
  | SKIP ; SEMICOLON { Skip }
  | x = ID ; ASSIGN ; aexpr = aexpr ; SEMICOLON { Assign (x, aexpr) }
  | ASSUME ; bexpr = bexpr ; SEMICOLON { Assume (bexpr) }
  | ASSERT ; bexpr = bexpr ; SEMICOLON { Assert (bexpr) }
  | INVOKE ; serv = ID ; LPAREN ; args = separated_list(COMMA, aexpr) ; RPAREN ; SEMICOLON
    { Invoke (serv, args) }
  | x = ID ; ASSIGN ; INVOKE ; serv = ID ; LPAREN ; args = separated_list(COMMA, aexpr) ; RPAREN ; SEMICOLON
    { AssignInvoke (x, serv, args) }

structured_stmt:
  | IF ; bexpr = bexpr ; THEN ; block1 = block ; ELSE ; block2 = block { If (bexpr, block1, block2) }
  | IF ; bexpr = bexpr ; THEN ; block = block { If (bexpr, block, Skip) }
  | WHILE ; bexpr = bexpr ; DO ; block = block { While (bexpr, block) }
