type typ = 
    | TInt
    | TBool
    | TOutcome
    | TErr
    | TArrow of typ list * typ
    | TCustom of string


type binop =
    | Add | Sub | Mul | Div
    | Lt | Le | Gt | Ge | Eq | Neq
    | Or | And

type unop =
    | Not

type aggrop =
    | Sum
    | Avg
    | Min
    | Max
    | Sorted

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


type policy_expr =
  | PExpr of expr
  | PAgg of aggrop * string
  | PBinOp of binop * policy_expr * policy_expr
  | PUnOp of unop * policy_expr

type regex =
    | RService of ident
    | RConcat of regex * regex
    | RChoice of regex * regex
    | RStar of regex

type policy = 
    | QosFieldOp of policy_expr
    | Regex of regex




(* QoS *)
type qos_def = (ident * typ) list
type qos_constraint = expr list

(* SLA *)
type sla = (ident * expr) list

(* Parameters and returns *)
type param = ident * typ
type ret = ident * typ

(* Conditions *)
type condition = expr

type service = {
    name : ident;
    params : param list;
    returns : ret list;
    sla : sla;
    precond : condition list;
    qos : qos_constraint;
    ok_post : condition list;
    err_post : condition list;
}

(* Function signatures: * -> int *)
type funtype = 
    | TArrow of typ * funtype
    | TBase of typ

type func_sig = {
    fname : ident;
    ty : funtype;
}

type program = {
    globals: global list;
    functions: func_sig list;
    policies: policy list;
    qos: qos_def;
    services: service list;
}


(* TODO: enforce invariants for contracts: 
- no duplicate service names
- regex use services that are defined in the program
- policies only use variables/fields defined in the program
- QoS constraints for each field of QoS vector
*)

let validate (p: program) : unit =
  let service_names = List.map (fun s -> s.name) p.services in
  let unique_service_names = List.sort_uniq String.compare service_names in
  if List.length service_names <> List.length unique_service_names then
    failwith "Duplicate service names found in the program"