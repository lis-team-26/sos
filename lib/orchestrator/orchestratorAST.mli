open Expr.AST
open Utils.Data
open Utils.Types

type stmt =
  | Skip
  | Declare of var_type * ident * expr
  | Assign of ident * expr
  | Assume of expr
  | Assert of expr * loc
  | Seq of stmt * stmt
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Invoke of ident * expr list * loc
  | DeclareInvoke of ident * ident * expr list * loc
  | AssignInvoke of ident * ident * expr list * loc
