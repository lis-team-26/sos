type typ = TInt | TBool | TArrow of typ list * typ
type binop = Add | Sub | Mul | Div | Lt | Le | Gt | Ge | Eq | Neq | Or | And
type unop = Not
type aggrop = Sum | Avg | Min | Max
type ident = string

type expr =
  | EInt of int
  | EBool of bool
  | EVar of ident
  | ESla
  | EField of expr * ident
  | EApp of ident * expr list
  | EBinOp of binop * expr * expr
  | EUnOp of unop * expr

type global = ident * typ

type serv2letter = (string * char) list
            
type policy_type =
  | QosFieldOp of aggrop * string * binop * int
  | Regex of serv2letter (*map from service to letter*) * string (*regex using those letters*)
  | Sort of ident

type policy = policy_type * string option

(* QoS *)
type qos_def = (ident * typ) list
type qos_constraint = expr list

(* SLA *)
type trust = int

(* Parameters and returns *)
type param = ident * typ
type ret = ident * typ

(* Conditions *)
type lhs = LVar of ident | LApp of ident * expr list
type effct = lhs * expr
type constrnt = binop * lhs * expr
type behavior = effct list * constrnt list
type condition = expr

type service = {
  name : ident;
  params : param list;
  returns : ret list;
  trust : trust;
  precond : condition list;
  qos : behavior;
  ok_post : behavior;
  err_post : behavior;
}

(* Function signatures: * -> int *)
type funtype = TArrow of typ * funtype | TBase of typ
type func_sig = { fname : ident; ty : funtype }

type contract = {
  globals : global list;
  functions : func_sig list;
  policies : policy list;
  qos : qos_def;
  services : service list;
}
