open Expr.AST

type ident = string
type fun_type = TFun of var_type list * var_type
type typed_var = ident * var_type
type typed_fun = ident * fun_type

type regex =
  | RService of ident
  | RConcat of regex * regex
  | RChoice of regex * regex
  | RStar of regex

type aggr_op = Sum | Avg | Min | Max

type policy_type =
  | QosFieldOp of aggr_op * string * bin_op * int
  | Regex of regex
  | Sort of ident

type policy = policy_type * string option

(* Conditions *)
type effct_lhs = LVar of ident | LApp of ident * expr list
type effct = effct_lhs * expr
type postcond = effct list * expr list

type service = {
  name : ident;
  params : typed_var list;
  returns : typed_var list;
  precond : expr list;
  qos_postcond : postcond;
  ok_postcond : postcond;
  err_postcond : postcond;
}

type contract = {
  globals : typed_var list;
  functions : typed_fun list;
  policies : policy list;
  qos : typed_var list;
  services : service list;
}
