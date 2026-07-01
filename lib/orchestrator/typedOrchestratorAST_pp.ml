open TypedOrchestratorAST
open Expr.TypedAST_pp
open Utils.Data_pp
open Utils.Loc

let rec pp_stmt fmt s =
  match s.it with
  | Skip -> Fmt.pf fmt "skip;"
  | Declare (x, AExpr e) -> Fmt.pf fmt "int %s := %a;" x pp_aexpr e
  | Declare (x, BExpr e) -> Fmt.pf fmt "bool %s := %a;" x pp_bexpr e
  | Assign (x, AExpr e) -> Fmt.pf fmt "%s := %a;" x pp_aexpr e
  | Assign (x, BExpr e) -> Fmt.pf fmt "%s := %a;" x pp_bexpr e
  | Assume e -> Fmt.pf fmt "assume %a;" pp_bexpr e
  | Assert e -> Fmt.pf fmt "assert %a;" pp_bexpr e
  | Seq (s1, s2) -> Fmt.pf fmt "%a@,%a" pp_stmt s1 pp_stmt s2
  | If (b, then_s, else_s) ->
      let else_s = match else_s.it with Skip -> None | _ -> Some else_s in
      let pp_else fmt else_s = Fmt.pf fmt " else %a" pp_block else_s in
      Fmt.pf fmt "if %a then %a%a" pp_bexpr b pp_block then_s
        (Fmt.option pp_else) else_s
  | While (b, body) -> Fmt.pf fmt "while %a do %a" pp_bexpr b pp_block body
  | Invoke (f, args) ->
      Fmt.pf fmt "invoke %s(%a);" f (pp_list_inline pp_typed_expr) args
  | DeclareInvoke (x, f, args) ->
      Fmt.pf fmt "rcpt %s := invoke %s(%a);" x f
        (pp_list_inline pp_typed_expr)
        args
  | AssignInvoke (x, f, args) ->
      Fmt.pf fmt "%s := invoke %s(%a);" x f (pp_list_inline pp_typed_expr) args

and pp_block fmt stmt = Fmt.pf fmt "{@,@[<v 2>  %a@]@,}" pp_stmt stmt

let pp_program = Fmt.vbox pp_stmt
