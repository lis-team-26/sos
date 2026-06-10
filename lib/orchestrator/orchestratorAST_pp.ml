open Format
open OrchestratorAST
open Expr.AST_pp
open Utils.Data

let rec pp_stmt fmt = function
  | Skip -> fprintf fmt "skip;"
  | Declare (t, x, e) -> fprintf fmt "%a %s := %a;" pp_var_type t x pp_expr e
  | Assign (x, e) -> fprintf fmt "%s := %a;" x pp_expr e
  | Assume e -> fprintf fmt "assume %a;" pp_expr e
  | Assert e -> fprintf fmt "assert %a;" pp_expr e
  | Seq (s1, s2) -> fprintf fmt "%a@,%a" pp_stmt s1 pp_stmt s2
  | If (e, then_s, else_s) -> (
      fprintf fmt "if %a then " pp_expr e;
      pp_block fmt then_s;
      match else_s with
      | Skip -> ()
      | _ -> fprintf fmt " else %a" pp_block else_s)
  | While (e, body) ->
      fprintf fmt "while %a do " pp_expr e;
      pp_block fmt body
  | Invoke (f, args) -> fprintf fmt "invoke %s(%a);" f pp_expr_list args
  | DeclareInvoke (x, f, args) ->
      fprintf fmt "rcpt %s := invoke %s(%a);" x f pp_expr_list args
  | AssignInvoke (x, f, args) ->
      fprintf fmt "%s := invoke %s(%a);" x f pp_expr_list args

and pp_block fmt stmt = fprintf fmt "{@,@[<v 2>  %a@]@,}" pp_stmt stmt

let pp_program fmt ast = fprintf fmt "@[<v>%a@]@." pp_stmt ast
