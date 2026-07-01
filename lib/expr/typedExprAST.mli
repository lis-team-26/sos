open Utils.Data
open Utils.Loc

(** Arithmetic binary operators. *)
type arithm_op = Add | Sub | Mul | Div

(** Comparison operators. *)
type cmp_op = Eq | Neq | Lt | Le | Gt | Ge

(** Boolean operators. *)
type bool_op = And | Or

(** Arithmetic field accessors for receipt values. *)
type arithm_field = AReturnValue | AQosField of ident

(** Boolean field accessors for receipt values. *)
type bool_field = BReturnValue | BSuccessful | BQosField of ident

type aexpr = aexpr_node located
(** Typed arithmetic expression, attached to its source code location. *)

and bexpr = bexpr_node located
(** Typed boolean expression, attached to its source code location. *)

(** Typed arithmetic expression nodes. *)
and aexpr_node =
  | ALit of int
  | AVar of ident
  | AAccess of ident * arithm_field
  | ANonDet
  | AOp of aexpr * arithm_op * aexpr
  | AApp of ident * typed_expr list

(** Typed boolean expression nodes. *)
and bexpr_node =
  | BLit of bool
  | BVar of ident
  | BAccess of ident * bool_field
  | BNonDet
  | BCmpOp of aexpr * cmp_op * aexpr
  | BBoolOp of bexpr * bool_op * bexpr
  | BNot of bexpr
  | BApp of ident * typed_expr list

(** Typed expression, either arithmetic or boolean. *)
and typed_expr = AExpr of aexpr | BExpr of bexpr
