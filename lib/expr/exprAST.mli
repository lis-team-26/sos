open Utils.Data
open Utils.Loc

(** Binary operators. *)
type bin_op = Add | Sub | Mul | Div | And | Or | Eq | Neq | Lt | Le | Gt | Ge

(** Unary operators. *)
type un_op = Not

(** Receipt field accessors. *)
type field = ReturnValue | Successful | QosField of ident

type expr = expr_node located
(** Untyped expression, attached to its source code location. *)

(** Untyped expression nodes. *)
and expr_node =
  | EInt of int
  | EBool of bool
  | EVar of ident
  | EAccess of ident * field
  | EIntNonDet
  | EBoolNonDet
  | EBinOp of expr * bin_op * expr
  | EUnOp of un_op * expr
  | EApp of ident * expr list
