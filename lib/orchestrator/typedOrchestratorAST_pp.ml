open Format
open TypedOrchestratorAST
open Expr.AST_pp
open Expr.TypedAST_pp

let rec pp_stmt fmt = function
  | Skip -> fprintf fmt "skip;"
  | Declare (x, AExpr e) -> fprintf fmt "int %s := %a;" x pp_aexpr e
  | Declare (x, BExpr e) -> fprintf fmt "bool %s := %a;" x pp_bexpr e
  | Assign (x, AExpr e) -> fprintf fmt "%s := %a;" x pp_aexpr e
  | Assign (x, BExpr e) -> fprintf fmt "%s := %a;" x pp_bexpr e
  | Assume e -> fprintf fmt "assume %a;" pp_bexpr e
  | Assert (e, _) -> fprintf fmt "assert %a;" pp_bexpr e
  | Seq (s1, s2) -> fprintf fmt "%a@,%a" pp_stmt s1 pp_stmt s2
  | If (e, then_s, else_s) -> (
      fprintf fmt "if %a then " pp_bexpr e;
      pp_block fmt then_s;
      match else_s with
      | Skip -> ()
      | _ -> fprintf fmt " else %a" pp_block else_s)
  | While (e, body) ->
      fprintf fmt "while %a do " pp_bexpr e;
      pp_block fmt body
  | Invoke (f, args, _) ->
      fprintf fmt "invoke %s(%a);" f pp_typed_expr_list args
  | DeclareInvoke (x, f, args, _) ->
      fprintf fmt "rcpt %s := invoke %s(%a);" x f pp_typed_expr_list args
  | AssignInvoke (x, f, args, _) ->
      fprintf fmt "%s := invoke %s(%a);" x f pp_typed_expr_list args

and pp_block fmt stmt = fprintf fmt "{@,@[<v 2>  %a@]@,}" pp_stmt stmt

let pp_program fmt ast = fprintf fmt "@[<v>%a@]@." pp_stmt ast
