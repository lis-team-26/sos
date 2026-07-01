open Expr.AST
open Utils.Data
open Utils.Loc
open Utils.Types

type stmt = stmt_node located
(** An untyped statement in the orchestrator language, attached to its source
    code location.*)

(** Untyped statements nodes. *)
and stmt_node =
  | Skip
  | Declare of var_type * ident * expr
  | Assign of ident * expr
  | Assume of expr
  | Assert of expr
  | Seq of stmt * stmt
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Invoke of ident * expr list
  | DeclareInvoke of ident * ident * expr list
  | AssignInvoke of ident * ident * expr list
