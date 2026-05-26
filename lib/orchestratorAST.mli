open Expr.AST

type stmt =
  | Skip
  | Declare of var_type * ident * expr
  | Assign of ident * expr
  | Seq of stmt * stmt
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Assume of expr
  | Assert of expr
  | Invoke of ident * expr list
  | AssignInvoke of ident * ident * expr list
