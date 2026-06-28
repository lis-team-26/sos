open ContractAST
open Expr.AST
open Expr.TypedAST

type policy_type =
  | QosFieldOp of aggr_op * string * cmp_op * int
  | Regex of serv2letter * string
  | Sort of ident

type policy = policy_type * string option
type effct_lhs = LVar of ident | LApp of ident * typed_expr list
type effct = effct_lhs * typed_expr
type postcond = effct list * bexpr list

type service = {
  name : ident;
  params : ident list;
  returns : typed_var;
  precond : bexpr list;
  qos_postcond : postcond;
  ok_postcond : postcond;
  err_postcond : postcond option;
}

type contract = {
  globals : typed_var list;
  globals_assumptions : bexpr list;
  functions : ident list;
  policies : policy list;
  qos : typed_var list;
  services : service list;
}
