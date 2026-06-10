type ident = string
type bin_op = Add | Sub | Mul | Div | And | Or | Eq | Neq | Lt | Le | Gt | Ge
type un_op = Not
type field = ReturnValue | Successful | QosField of ident

type expr =
  | EInt of int
  | EBool of bool
  | EVar of ident
  | EAccess of ident * field
  | EIntNonDet
  | EBoolNonDet
  | EBinOp of expr * bin_op * expr
  | EUnOp of un_op * expr
  | EApp of ident * expr list
