open Utils.Data

type arithm_op = Add | Sub | Mul | Div
type cmp_op = Eq | Neq | Lt | Le | Gt | Ge
type bool_op = And | Or

type aexpr =
  | ALit of int
  | AVar of ident
  | ANonDet
  | AOp of aexpr * arithm_op * aexpr
  | AApp of ident * typed_expr list

and bexpr =
  | BLit of bool
  | BVar of ident
  | BNonDet
  | BCmpOp of aexpr * cmp_op * aexpr
  | BBoolOp of bexpr * bool_op * bexpr
  | BNot of bexpr
  | BApp of ident * typed_expr list

and typed_expr = AExpr of aexpr | BExpr of bexpr
