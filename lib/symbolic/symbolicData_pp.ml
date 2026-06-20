open Format
open SymbolicData
open Utils.Data
open Utils.Data_pp
open Soteria.Tiny_values

let rec pp_svalue_rec fmt exp =
  match Svalue.kind exp with
  | Svalue.Var v ->
      let type_chr =
        if Svalue.is_bool_ty (exp |> Typed.type_ |> Typed.get_ty) then 'B'
        else 'I'
      in
      fprintf fmt "%c[%d]" type_chr (Soteria.Symex.Var.to_int v)
  | Svalue.Bool b -> fprintf fmt (if b then "T" else "F")
  | Svalue.Int i -> fprintf fmt "%d" (i |> Z.to_int)
  | Svalue.Unop (op, expr) -> (
      match op with Not -> fprintf fmt "not(%a)" pp_svalue_rec expr)
  | Svalue.Binop (op, expr1, expr2) ->
      fprintf fmt "(%a %s %a)" pp_svalue_rec expr1
        (match op with
        | And -> "∧"
        | Or -> "v"
        | Eq -> "="
        | Leq -> "<="
        | Lt -> "<"
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
        | Div -> "/"
        | Rem -> "rem"
        | Mod -> "mod")
        pp_svalue_rec expr2
  | Svalue.Nop (op, exprList) -> (
      match op with
      | Distinct ->
          fprintf fmt "distinct(%a)" (pp_list_inline pp_svalue_rec) exprList)
  | Svalue.Ite (expr1, expr2, expr3) ->
      fprintf fmt "(if %a then %a else %a)" pp_svalue_rec expr1 pp_svalue_rec
        expr2 pp_svalue_rec expr3

let pp_svalue fmt svalue =
  let expr = Typed.Expr.of_value svalue in
  pp_svalue_rec fmt expr

let rec pp_value fmt = function
  | SymbInt v -> pp_svalue fmt v
  | SymbBool v -> pp_svalue fmt v
  | SymbReceipt { ret_val; successful; qos_fields } ->
      fprintf fmt
        "receipt {@,\
         @[<v 2>  retval = %a;@,\
         successful = %a;@,\
         qos = {@,\
         @[<v 2>  %a@]@,\
         }@]@,\
         }"
        pp_value ret_val pp_value (SymbBool successful)
        (pp_env Fmt.string pp_value)
        qos_fields
