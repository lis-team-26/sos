open Format
open SymbolicData
open Utils.Data
open Utils.Data_pp

let pp_svalue fmt svalue =
  let open Soteria.Tiny_values.Svalue in
  let is_atomic svalue =
    match kind svalue with Var _ | Bool _ | Int _ -> true | _ -> false
  in
  let pp_bin_op fmt =
    let open Binop in
    function
    | And -> Fmt.pf fmt "∧"
    | Or -> Fmt.pf fmt "v"
    | Eq -> Fmt.pf fmt "="
    | Leq -> Fmt.pf fmt "<="
    | Lt -> Fmt.pf fmt "<"
    | Plus -> Fmt.pf fmt "+"
    | Minus -> Fmt.pf fmt "-"
    | Times -> Fmt.pf fmt "*"
    | Div -> Fmt.pf fmt "/"
    | Rem -> Fmt.pf fmt "rem"
    | Mod -> Fmt.pf fmt "mod"
  in
  let rec pp_with_parens fmt e =
    if is_atomic e then pp fmt e else Fmt.pf fmt "(%a)" pp e
  and pp fmt e =
    match kind e with
    | Var v ->
        let var_type =
          if e |> Typed.type_ |> Typed.get_ty |> is_bool_ty then 'B' else 'I'
        in
        Fmt.pf fmt "%c[%d]" var_type (Soteria.Symex.Var.to_int v)
    | Bool b -> Fmt.bool fmt b
    | Int i -> Fmt.int fmt @@ Z.to_int i
    | Unop (Not, e) -> (
        match kind e with
        | Binop (Eq, e1, e2) ->
            Fmt.pf fmt "%a != %a" pp_with_parens e1 pp_with_parens e2
        | _ -> Fmt.pf fmt "!%a" pp_with_parens e)
    | Binop (op, e1, e2) ->
        Fmt.pf fmt "%a %a %a" pp_with_parens e1 pp_bin_op op pp_with_parens e2
    | Nop (Distinct, exprs) ->
        Fmt.pf fmt "distinct(%a)" (pp_list_inline pp_with_parens) exprs
    | Ite (e1, e2, e3) ->
        Fmt.pf fmt "if %a then %a else %a" pp_with_parens e1 pp_with_parens e2
          pp_with_parens e3
  in
  pp_with_parens fmt @@ Typed.Expr.of_value svalue

let rec pp_value fmt = function
  | SymbInt v -> pp_svalue fmt v
  | SymbBool v -> pp_svalue fmt v
  | SymbReceipt { ret_val; successful; qos_fields } ->
      let pp fmt (ret_val, successful, qos_fields) =
        Fmt.pf fmt "retval: %a;@,successful: %a;@,%a" pp_value ret_val pp_value
          (SymbBool successful)
          (pp_field ~name:"qos" (pp_env Fmt.string pp_value))
          qos_fields
      in
      pp_field ~name:"receipt" pp fmt (ret_val, successful, qos_fields)
