open OrchestratorAST
open Expr.AST_pp
open Utils.Data_pp
open Utils.Loc
open Utils.Types_pp

let rec pp_stmt fmt { it = stmt } =
  match stmt with
  | Skip -> Fmt.pf fmt "skip;"
  | Declare (t, x, e) -> Fmt.pf fmt "%a %s := %a;" pp_var_type t x pp_expr e
  | Assign (x, e) -> Fmt.pf fmt "%s := %a;" x pp_expr e
  | Assume e -> Fmt.pf fmt "assume %a;" pp_expr e
  | Assert e -> Fmt.pf fmt "assert %a;" pp_expr e
  | Seq (s1, s2) -> Fmt.pf fmt "%a@,%a" pp_stmt s1 pp_stmt s2
  | If (e, then_s, else_s) ->
      let else_s = match else_s.it with Skip -> None | _ -> Some else_s in
      let pp_else fmt else_s = Fmt.pf fmt " else %a" pp_block else_s in
      Fmt.pf fmt "if %a then %a%a" pp_expr e pp_block then_s
        (Fmt.option pp_else) else_s
  | While (e, body) -> Fmt.pf fmt "while %a do %a" pp_expr e pp_block body
  | Invoke (f, args) ->
      Fmt.pf fmt "invoke %s(%a);" f (pp_list_inline pp_expr) args
  | DeclareInvoke (x, f, args) ->
      Fmt.pf fmt "rcpt %s := invoke %s(%a);" x f (pp_list_inline pp_expr) args
  | AssignInvoke (x, f, args) ->
      Fmt.pf fmt "%s := invoke %s(%a);" x f (pp_list_inline pp_expr) args

and pp_block fmt stmt = Fmt.pf fmt "{@,@[<v 2>  %a@]@,}" pp_stmt stmt

let pp_program = Fmt.vbox pp_stmt
