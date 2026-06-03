open Expr.AST

type stmt =
  | Skip
  | Declare of var_type * ident * expr
  | Assign of ident * expr
  | Assume of expr
  | Assert of expr
  | Seq of stmt * stmt
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Invoke of ident * expr list
  | DeclareInvoke of var_type * ident * ident * expr list
  | AssignInvoke of ident * ident * expr list
