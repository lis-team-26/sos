%{
  open SimpleAST
%}

%token <int> INT
%token <string> ID
%token <bool> BOOL
%token NONDET
%token DOT
%token ADD SUB MUL DIV
%token AND OR NOT
%token EQ NEQ LT LE GT GE
%token SERVICE POLICY BADPREFIX MAXSUM MAX COST LATENCY THRUST COLON
%token SKIP ASSIGN SEMICOLON IF THEN ELSE WHILE DO
%token ASSUME ASSERT INVOKE
%token COMMA LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK EOF

%start <decl list * stmt> prog

%left OR
%left AND
%nonassoc NOT
%left ADD SUB
%left MUL DIV
%left DOT

%%

prog:
  | d = list(decl) ; SEMICOLON ; stmt = sequence(stmt) EOF { (d, stmt) }
  | stmt = sequence(stmt) EOF { ([], stmt) }

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

sequence(X):
  | x = X ; y = sequence(X) { Seq(x, y) }
  | x = X { x }

pre: (* the precondition can be expressed with assume and assert*)
  | ASSUME ; bexpr = bexpr ; SEMICOLON { Assume (bexpr) }
  | ASSERT ; bexpr = bexpr ; SEMICOLON { Assert (bexpr) }
  | SKIP ; SEMICOLON { Skip }

post: (* the postcondition can be expressed with assume and assignment*)
  | ASSUME ; bexpr = bexpr ; SEMICOLON { Assume (bexpr) }
  | x = ID ; ASSIGN ; aexpr = aexpr ; SEMICOLON { Assign (x, aexpr) }
  | SKIP ; SEMICOLON { Skip }

thrust:
  | THRUST ; COLON ; t = INT { t }

mcost_mlat:
  | MAX ; COST ; COLON ; c = INT ; MAX ; LATENCY ; COLON ; l = INT { (c, l) }
  | MAX ; LATENCY ; COLON ; l = INT ; MAX ; COST ; COLON ; c = INT { (c, l) }

mcost_mlat_thrust:
  | m = mcost_mlat ; t = thrust { (m, t) }
  | t = thrust ; m = mcost_mlat { (m, t) }

decl:
  | SERVICE ; name = ID ; LPAREN ; l = separated_list(COMMA, ID) ; RPAREN ; d = mcost_mlat_thrust ; LBRACE ; pr = sequence(pre) ; RBRACE ; LBRACE ; po = sequence(post) ; RBRACE { Service (name, l, Seq (pr, po), fst(fst(d)), snd(fst(d)), snd(d)) }
  | POLICY ; p = policy ; LBRACK ; l = separated_list(COMMA, ID) ; RBRACK { Policy (p, l) }

regex:
  | s = ID { Service s }
  | r1 = regex ; ADD ; r2 = regex { Choice (r1, r2) }
  | r1 = regex ; DOT ; r2 = regex { Concat (r1, r2) }
  | r = regex ; MUL { Star r }
  | LPAREN ; r = regex ; RPAREN { r }

qos:
  | LATENCY { Cost }
  | COST { Latency }

policy:
  | THRUST { Thrust }
  | BADPREFIX ; r = regex { BadPref r }
  | MAXSUM ; f = qos ; n = INT { MaxSum (f, n) }

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
