open Expr.TypedAST
open Utils.Data

type stmt =
  | Skip
  | Declare of ident * typed_expr
  | Assign of ident * typed_expr
  | Seq of stmt * stmt
  | If of bexpr * stmt * stmt
  | While of bexpr * stmt
  | Assume of bexpr
  | Assert of bexpr * int (*add line number*)
  | Invoke of ident * typed_expr list
  | DeclareInvoke of ident * ident * typed_expr list
  | AssignInvoke of ident * ident * typed_expr list
