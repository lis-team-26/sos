open TypedExprAST
open Utils.Data_pp
open Utils.Loc

let pp_arithm_op fmt = function
  | Add -> Fmt.pf fmt "+"
  | Sub -> Fmt.pf fmt "-"
  | Mul -> Fmt.pf fmt "*"
  | Div -> Fmt.pf fmt "/"

let pp_cmp_op fmt = function
  | Eq -> Fmt.pf fmt "=="
  | Neq -> Fmt.pf fmt "!="
  | Lt -> Fmt.pf fmt "<"
  | Le -> Fmt.pf fmt "<="
  | Gt -> Fmt.pf fmt ">"
  | Ge -> Fmt.pf fmt ">="

let pp_bool_op fmt = function And -> Fmt.pf fmt "&&" | Or -> Fmt.pf fmt "||"

let rec pp_aexpr fmt e =
  let is_atomic = function AOp _ -> false | _ -> true in
  let pp_with_parens fmt e =
    if is_atomic e.it then pp_aexpr fmt e else Fmt.pf fmt "(%a)" pp_aexpr e
  in
  match e.it with
  | ALit i -> Fmt.pf fmt "%d" i
  | AVar v -> Fmt.pf fmt "%s" v
  | AAccess (x, AReturnValue) -> Fmt.pf fmt "%s.retval" x
  | AAccess (x, AQosField f) -> Fmt.pf fmt "%s.qos.%s" x f
  | ANonDet -> Fmt.pf fmt "int?"
  | AOp (e1, op, e2) ->
      Fmt.pf fmt "%a %a %a" pp_with_parens e1 pp_arithm_op op pp_with_parens e2
  | AApp (f, args) -> Fmt.pf fmt "%s(%a)" f (pp_list_inline pp_typed_expr) args

and pp_bexpr fmt e =
  let is_atomic = function
    | BNot _ | BCmpOp _ | BBoolOp _ -> false
    | _ -> true
  in
  let pp_with_parens fmt e =
    if is_atomic e.it then pp_bexpr fmt e else Fmt.pf fmt "(%a)" pp_bexpr e
  in
  match e.it with
  | BLit b -> Fmt.pf fmt "%b" b
  | BVar v -> Fmt.pf fmt "%s" v
  | BAccess (x, BReturnValue) -> Fmt.pf fmt "%s.retval" x
  | BAccess (x, BSuccessful) -> Fmt.pf fmt "%s.successful" x
  | BAccess (x, BQosField f) -> Fmt.pf fmt "%s.qos.%s" x f
  | BNonDet -> Fmt.pf fmt "bool?"
  | BCmpOp (e1, op, e2) ->
      Fmt.pf fmt "%a %a %a" pp_aexpr e1 pp_cmp_op op pp_aexpr e2
  | BBoolOp (e1, op, e2) ->
      Fmt.pf fmt "%a %a %a" pp_with_parens e1 pp_bool_op op pp_with_parens e2
  | BNot e -> Fmt.pf fmt "!%a" pp_with_parens e
  | BApp (f, args) -> Fmt.pf fmt "%s(%a)" f (pp_list_inline pp_typed_expr) args

and pp_typed_expr fmt = function
  | AExpr e -> pp_aexpr fmt e
  | BExpr e -> pp_bexpr fmt e
