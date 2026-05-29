%{
  open Expr.AST
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR

%token NONDET
%token PLUS MINUS TIMES DIV
%token LT LE GT GE EQ NEQ
%token AND OR NOT
%token LPAREN RPAREN COMMA

%left OR
%left AND
%nonassoc NOT
%left LT LE GT GE EQ NEQ
%left PLUS MINUS
%left TIMES DIV

%%

atom_expr(extra_atom):
  | n = INT { EInt n }
  | b = BOOL { EBool b }
  | v = VAR { EVar v }
  | a = extra_atom { a }

%public expr(extra_atom):
  | a = atom_expr(extra_atom) { a }
  | e1 = expr(extra_atom) ; op = PLUS ; e2 = expr(extra_atom) { EBinOp (e1, Add, e2) }
  | e1 = expr(extra_atom) ; op = MINUS ; e2 = expr(extra_atom) { EBinOp (e1, Sub, e2) }
  | e1 = expr(extra_atom) ; op = TIMES ; e2 = expr(extra_atom) { EBinOp (e1, Mul, e2) }
  | e1 = expr(extra_atom) ; op = DIV ; e2 = expr(extra_atom) { EBinOp (e1, Div, e2) }
  | e1 = expr(extra_atom) ; op = AND ; e2 = expr(extra_atom) { EBinOp (e1, And, e2) }
  | e1 = expr(extra_atom) ; op = OR ; e2 = expr(extra_atom) { EBinOp (e1, Or, e2) }
  | e1 = expr(extra_atom) ; op = EQ ; e2 = expr(extra_atom) { EBinOp (e1, Eq, e2) }
  | e1 = expr(extra_atom) ; op = NEQ ; e2 = expr(extra_atom) { EBinOp (e1, Neq, e2) }
  | e1 = expr(extra_atom) ; op = LT ; e2 = expr(extra_atom) { EBinOp (e1, Lt, e2) }
  | e1 = expr(extra_atom) ; op = LE ; e2 = expr(extra_atom) { EBinOp (e1, Le, e2) }
  | e1 = expr(extra_atom) ; op = GT ; e2 = expr(extra_atom) { EBinOp (e1, Gt, e2) }
  | e1 = expr(extra_atom) ; op = GE ; e2 = expr(extra_atom) { EBinOp (e1, Ge, e2) }
  | op = NOT ; e = expr(extra_atom) { EUnOp (Not, e) }
  | LPAREN ; e = expr(extra_atom) ; RPAREN { e }

%public exprs(extra_atom):
  | es = separated_list(COMMA, expr(extra_atom)) { es }