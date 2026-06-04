open ExprAST
open Format
open Utils.Data

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
  | EIntNonDet -> fprintf fmt "int?"
  | EBoolNonDet -> fprintf fmt "bool?"
  | EUnOp (op, e) -> fprintf fmt "(%a%a)" pp_un_op op pp_expr e
  | EApp (f, args) -> fprintf fmt "%s(%a)" f pp_expr_list args
  | EBinOp (e1, op, e2) ->
      fprintf fmt "(%a %a %a)" pp_expr e1 pp_bin_op op pp_expr e2

and pp_expr_list fmt = function
  | [] -> ()
  | [ e ] -> pp_expr fmt e
  | e :: es -> fprintf fmt "%a, %a" pp_expr e pp_expr_list es

let rec pp_var_type fmt = function
  | TInt -> fprintf fmt "int"
  | TBool -> fprintf fmt "bool"
