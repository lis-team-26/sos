open ExprAST
open Utils.Data_pp
open Utils.Loc

let pp_bin_op fmt = function
  | Add -> Fmt.pf fmt "+"
  | Sub -> Fmt.pf fmt "-"
  | Mul -> Fmt.pf fmt "*"
  | Div -> Fmt.pf fmt "/"
  | Or -> Fmt.pf fmt "||"
  | And -> Fmt.pf fmt "&&"
  | Lt -> Fmt.pf fmt "<"
  | Le -> Fmt.pf fmt "<="
  | Gt -> Fmt.pf fmt ">"
  | Ge -> Fmt.pf fmt ">="
  | Eq -> Fmt.pf fmt "=="
  | Neq -> Fmt.pf fmt "!="

let pp_un_op fmt = function Not -> Fmt.pf fmt "!"

let rec pp_expr fmt { it = e } =
  let is_atomic = function EUnOp _ | EBinOp _ -> false | _ -> true in
  let pp_with_parens fmt e =
    if is_atomic e.it then pp_expr fmt e else Fmt.pf fmt "(%a)" pp_expr e
  in
  match e with
  | EInt i -> Fmt.pf fmt "%d" i
  | EBool b -> Fmt.pf fmt "%b" b
  | EVar v -> Fmt.pf fmt "%s" v
  | EAccess (x, ReturnValue) -> Fmt.pf fmt "%s.retval" x
  | EAccess (x, Successful) -> Fmt.pf fmt "%s.successful" x
  | EAccess (x, QosField f) -> Fmt.pf fmt "%s.qos.%s" x f
  | EIntNonDet -> Fmt.pf fmt "int?"
  | EBoolNonDet -> Fmt.pf fmt "bool?"
  | EApp (f, args) -> Fmt.pf fmt "%s(%a)" f (pp_list_inline pp_with_parens) args
  | EUnOp (op, e) -> Fmt.pf fmt "%a%a" pp_un_op op pp_with_parens e
  | EBinOp (e1, op, e2) ->
      Fmt.pf fmt "%a %a %a" pp_with_parens e1 pp_bin_op op pp_with_parens e2
