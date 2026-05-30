open Expr.AST

type fun_type = TFun of var_type list * var_type
type typed_var = ident * var_type
type typed_fun = ident * fun_type
type aggr_op = Sum | Avg | Min | Max
type serv2letter = (string * char) list

type policy_type =
  | QosFieldOp of aggr_op * string * bin_op * int
  | Regex of
      serv2letter (*map from service to letter*)
      * string (*regex using those letters*)
  | Sort of ident

type policy = policy_type * string option

(* Conditions *)
type effct_lhs = LVar of ident | LApp of ident * expr list
type effct = effct_lhs * expr
type postcond = effct list * expr list

type service = {
  name : ident;
  params : typed_var list;
  returns : typed_var;
  precond : expr list;
  qos_postcond : postcond;
  ok_postcond : postcond;
  err_postcond : postcond option;
}

type contract = {
  globals : typed_var list;
  functions : typed_fun list;
  policies : policy list;
  qos : typed_var list;
  services : service list;
}
