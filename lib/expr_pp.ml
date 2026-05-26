open ExprAST
open Format

let rec pp_var_type fmt = function
  | TInt -> fprintf fmt "int"
  | TBool -> fprintf fmt "bool"

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
  | ENonDet -> fprintf fmt "?"
  | EUnOp (op, e) -> fprintf fmt "(%a%a)" pp_un_op op pp_expr e
  | EApp (f, args) ->
      fprintf fmt "%s(" f;
      pp_expr_list fmt args;
      fprintf fmt ")"
  | EBinOp (e1, op, e2) ->
      fprintf fmt "(";
      pp_expr fmt e1;
      fprintf fmt " %a " pp_bin_op op;
      pp_expr fmt e2;
      fprintf fmt ")"

and pp_expr_list fmt = function
  | [] -> ()
  | [ e ] -> pp_expr fmt e
  | e :: es ->
      pp_expr fmt e;
      fprintf fmt ", ";
      pp_expr_list fmt es
