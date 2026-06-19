open ExprAST
open Format
open Utils.Data
open Utils.Data_pp
open Utils.Types

let pp_bin_op fmt = function
  | Add -> fprintf fmt "+"
  | Sub -> fprintf fmt "-"
  | Mul -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Or -> fprintf fmt "||"
  | And -> fprintf fmt "&&"
  | Lt -> fprintf fmt "<"
  | Le -> fprintf fmt "<="
  | Gt -> fprintf fmt ">"
  | Ge -> fprintf fmt ">="
  | Eq -> fprintf fmt "=="
  | Neq -> fprintf fmt "!="

let pp_un_op fmt = function Not -> fprintf fmt "!"

let rec pp_expr fmt = function
  | EInt i -> fprintf fmt "%d" i
  | EBool b -> fprintf fmt "%b" b
  | EVar v -> fprintf fmt "%s" v
  | EAccess (x, field) -> (
      match field with
      | ReturnValue -> fprintf fmt "%s.retval" x
      | Successful -> fprintf fmt "%s.successful" x
      | QosField f -> fprintf fmt "%s.qos.%s" x f)
  | EIntNonDet -> fprintf fmt "int?"
  | EBoolNonDet -> fprintf fmt "bool?"
  | EUnOp (op, e) -> fprintf fmt "(%a%a)" pp_un_op op pp_expr e
  | EApp (f, args) -> fprintf fmt "%s(%a)" f pp_expr_list args
  | EBinOp (e1, op, e2) ->
      fprintf fmt "(%a %a %a)" pp_expr e1 pp_bin_op op pp_expr e2

and pp_expr_list fmt = pp_list_inline pp_expr fmt
