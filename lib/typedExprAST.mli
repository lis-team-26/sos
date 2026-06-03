open Utils.Data

type arithm_op = TAdd | TSub | TMul | TDiv
type cmp_op = TEq | TNeq | TLt | TLe | TGt | TGe
type bool_op = TAnd | TOr

type aexpr =
  | ALit of int
  | AVar of ident
  | ANonDet
  | AOp of aexpr * arithm_op * aexpr
  | AApp of ident * expr list

and bexpr =
  | BLit of bool
  | BVar of ident
  | BNonDet
  | BCmpOp of aexpr * cmp_op * aexpr
  | BBoolOp of bexpr * bool_op * bexpr
  | BNot of bexpr
  | BApp of ident * expr list

and expr = AExpr of aexpr | BExpr of bexpr
