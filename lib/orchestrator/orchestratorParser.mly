%{
  open OrchestratorAST
  open Utils.Loc
  open Utils.Types
%}

%token ANONDET BNONDET
%token SKIP ASSIGN SEMICOLON IF THEN ELSE WHILE DO
%token ASSUME ASSERT INVOKE
%token DOT LBRACE RBRACE EOF
%token INT_TYPE BOOL_TYPE RECEIPT_TYPE
%token RETVAL SUCCESSFUL QOS

%start <stmt> program

%%

var_type:
  | INT_TYPE { TInt }
  | BOOL_TYPE { TBool }

orchestrator_atom_expr:
  | ANONDET { EIntNonDet |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | BNONDET { EBoolNonDet |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | f = VAR ; LPAREN ; args = exprs(orchestrator_atom_expr) ; RPAREN
    { EApp (f, args) |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | x = VAR ; DOT ; field = RETVAL
    { EAccess (x, ReturnValue) |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | x = VAR ; DOT ; field = SUCCESSFUL
    { EAccess (x, Successful) |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | x = VAR ; DOT ; field = QOS ; DOT ; f = VAR
    { EAccess (x, QosField f) |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }

orchestrator_expr:
  | e = expr(orchestrator_atom_expr) { e }

orchestrator_exprs:
  | es = exprs(orchestrator_atom_expr) { es }

stmt:
  | s = atom_stmt { s |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | s = structured_stmt { s |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }

stmts:
  | s = stmt ; ss = stmts { Seq (s, ss) |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | s = stmt { s }

atom_stmt:
  | SKIP ; SEMICOLON { Skip }
  | t = var_type ; x = VAR ; ASSIGN ; e = orchestrator_expr ; SEMICOLON { Declare (t, x, e) }
  | x = VAR ; ASSIGN ; e = orchestrator_expr ; SEMICOLON { Assign (x, e) }
  | ASSUME ; e = orchestrator_expr ; SEMICOLON { Assume e }
  | ASSERT ; e = orchestrator_expr ; SEMICOLON { Assert e }
  | INVOKE ; f = VAR ; LPAREN ; args = orchestrator_exprs ; RPAREN ; SEMICOLON
    { Invoke (f, args) }
  | RECEIPT_TYPE ; x = VAR ; ASSIGN ; INVOKE ; f = VAR ; LPAREN ; args = orchestrator_exprs ; RPAREN ; SEMICOLON
    { DeclareInvoke (x, f, args) }
  | x = VAR ; ASSIGN ; INVOKE ; f = VAR ; LPAREN ; args = orchestrator_exprs ; RPAREN ; SEMICOLON
    { AssignInvoke (x, f, args) }

structured_stmt:
  | IF ; e = orchestrator_expr ; THEN ; b1 = block ; ELSE ; b2 = block { If (e, b1, b2) }
  | IF ; e = orchestrator_expr ; THEN ; b = block
    { let loc = match b.at with
        | Loc l -> Loc { l with start_pos = l.end_pos }
        | EOFLoc -> EOFLoc
      in
      If (e, b, located ~loc Skip) }
  | WHILE ; e = orchestrator_expr ; DO ; b = block { While (e, b) }

block:
  | s = atom_stmt { s |> located_with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | LBRACE ; ss = stmts ; RBRACE { ss }

program:
  | ss = stmts EOF { ss }
