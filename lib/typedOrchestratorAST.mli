open TypedExpr.AST
open Utils.Data

type stmt =
  | Skip
  | Assign of ident * typed_expr
  | Seq of stmt * stmt
  | If of bexpr * stmt * stmt
  | While of bexpr * stmt
  | Assume of bexpr
  | Assert of bexpr
  | Invoke of ident * typed_expr list
  | AssignInvoke of ident * ident * typed_expr list
