open Expr.TypedAST
open Utils.Data
open Utils.Loc

type stmt = stmt_node located
(** A typed statement in the orchestrator language, attached to its source code
    location.*)

(** Typed statements nodes. *)
and stmt_node =
  | Skip
  | Declare of ident * typed_expr
  | Assign of ident * typed_expr
  | Seq of stmt * stmt
  | If of bexpr * stmt * stmt
  | While of bexpr * stmt
  | Assume of bexpr
  | Assert of bexpr
  | Invoke of ident * typed_expr list
  | DeclareInvoke of ident * ident * typed_expr list
  | AssignInvoke of ident * ident * typed_expr list
