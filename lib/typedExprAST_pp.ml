open TypedExprAST
open Format

let pp_aop fmt = function
  | TAdd -> fprintf fmt "+"
  | TSub -> fprintf fmt "-"
  | TMul -> fprintf fmt "*"
  | TDiv -> fprintf fmt "/"

let pp_cmp_op fmt = function
  | TEq -> fprintf fmt "=="
  | TNeq -> fprintf fmt "!="
  | TLt -> fprintf fmt "<"
  | TLe -> fprintf fmt "<="
  | TGt -> fprintf fmt ">"
  | TGe -> fprintf fmt ">="

let pp_bop fmt = function TAnd -> fprintf fmt "&&" | TOr -> fprintf fmt "||"

let rec pp_aexpr fmt = function
  | ALit i -> fprintf fmt "%d" i
  | AVar v -> fprintf fmt "%s" v
  | ANonDet -> fprintf fmt "int?"
  | AOp (e1, op, e2) ->
      fprintf fmt "(%a %a %a)" pp_aexpr e1 pp_aop op pp_aexpr e2
  | AApp (f, args) -> fprintf fmt "%s(%a)" f pp_typed_expr_list args

and pp_bexpr fmt = function
  | BLit b -> fprintf fmt "%b" b
  | BVar v -> fprintf fmt "%s" v
  | BNonDet -> fprintf fmt "bool?"
  | BCmpOp (e1, op, e2) ->
      fprintf fmt "(%a %a %a)" pp_aexpr e1 pp_cmp_op op pp_aexpr e2
  | BBoolOp (e1, op, e2) ->
      fprintf fmt "(%a %a %a)" pp_bexpr e1 pp_bop op pp_bexpr e2
  | BNot e -> fprintf fmt "(!%a)" pp_bexpr e
  | BApp (f, args) -> fprintf fmt "%s(%a)" f pp_typed_expr_list args

and pp_typed_expr fmt = function
  | AExpr e -> pp_aexpr fmt e
  | BExpr e -> pp_bexpr fmt e

and pp_typed_expr_list fmt = function
  | [] -> ()
  | [ e ] -> pp_typed_expr fmt e
  | e :: es -> fprintf fmt "%a, %a" pp_typed_expr e pp_typed_expr_list es
