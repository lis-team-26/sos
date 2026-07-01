open ExprAST
module T = TypedExprAST
open Utils.Data
open Utils.Loc
open Utils.Result
open Utils.Scope
open Utils.Types
open Result.Syntax

let rec type_check_arithm ~scope ~fun_env { it = e; at = loc } =
  let* typed_e =
    match e with
    | EInt n -> Ok (T.ALit n)
    | EBool _ ->
        located_error ~loc
          "Expected arithmetic expression but found boolean literal"
    | EVar v -> (
        match lookup v scope with
        | Some TInt -> Ok (T.AVar v)
        | Some TBool ->
            located_error ~loc
              "Variable %s expected arithmetic but found boolean" v
        | Some (TReceipt _) ->
            located_error ~loc
              "Variable %s expected arithmetic but found receipt" v
        | None -> located_error ~loc "Arithmetic variable %s not found" v)
    | EAccess (x, field) -> (
        match (lookup x scope, field) with
        | Some (TReceipt { ret_type; qos_types }), ReturnValue -> (
            match ret_type with
            | TInt -> Ok (T.AAccess (x, T.AReturnValue))
            | TBool ->
                located_error ~loc
                  "Field access on %s expected arithmetic but found boolean \
                   return value"
                  x
            | TReceipt _ -> failwith "Unreachable")
        | Some (TReceipt { ret_type; qos_types }), Successful ->
            located_error ~loc
              "Field access on %s expected arithmetic but found successful \
               field access"
              x
        | Some (TReceipt { ret_type; qos_types }), QosField f -> (
            match StringMap.find_opt f qos_types with
            | Some TInt -> Ok (T.AAccess (x, T.AQosField f))
            | Some TBool ->
                located_error ~loc
                  "Field access on %s expected arithmetic but found boolean \
                   qos_types field %s"
                  x f
            | Some (TReceipt _) -> failwith "Unreachable"
            | None ->
                located_error ~loc
                  "Field access on %s expected arithmetic but qos_types field \
                   %s not found"
                  x f)
        | Some TInt, _ ->
            located_error ~loc
              "Variable %s expected receipt but found arithmetic" x
        | Some TBool, _ ->
            located_error ~loc "Variable %s expected receipt but found boolean"
              x
        | None, _ -> located_error ~loc "Variable %s not found" x)
    | EIntNonDet -> Ok T.ANonDet
    | EBoolNonDet ->
        located_error ~loc
          "Expected arithmetic expression but found boolean nondet"
    | EBinOp (e1, op, e2) -> (
        let* typed_e1 = type_check_arithm ~scope ~fun_env e1 in
        let* typed_e2 = type_check_arithm ~scope ~fun_env e2 in
        match op with
        | Add -> Ok (T.AOp (typed_e1, T.Add, typed_e2))
        | Sub -> Ok (T.AOp (typed_e1, T.Sub, typed_e2))
        | Mul -> Ok (T.AOp (typed_e1, T.Mul, typed_e2))
        | Div -> Ok (T.AOp (typed_e1, T.Div, typed_e2))
        | And | Or | Eq | Neq | Lt | Le | Gt | Ge ->
            located_error ~loc
              "Expected arithmetic expression but found boolean operator")
    | EUnOp (Not, _) ->
        located_error ~loc
          "Expected arithmetic expression but found unary boolean operator"
    | EApp (f, args) -> (
        match StringMap.find_opt f fun_env with
        | Some (TFun (_, TInt)) ->
            let* args = type_check_app ~scope ~fun_env ~loc f args in
            Ok (T.AApp (f, args))
        | Some (TFun (_, TBool)) ->
            located_error ~loc
              "Function %s returns boolean but arithmetic expected" f
        | Some (TFun (_, TReceipt _)) -> failwith "Unreachable"
        | None -> located_error ~loc "Function %s not found" f)
  in
  Ok (located typed_e ~loc)

and type_check_bool ~scope ~fun_env { it = e; at = loc } =
  let* typed_e =
    match e with
    | EBool b -> Ok (T.BLit b)
    | EInt _ ->
        located_error ~loc
          "Expected boolean expression but found arithmetic literal"
    | EVar v -> (
        match lookup v scope with
        | Some TBool -> Ok (T.BVar v)
        | Some TInt ->
            located_error ~loc
              "Variable %s expected boolean but found arithmetic" v
        | Some (TReceipt _) ->
            located_error ~loc "Variable %s expected boolean but found receipt"
              v
        | None -> located_error ~loc "Boolean variable %s not found" v)
    | EAccess (x, field) -> (
        match (lookup x scope, field) with
        | Some (TReceipt { ret_type; qos_types }), ReturnValue -> (
            match ret_type with
            | TBool -> Ok (T.BAccess (x, T.BReturnValue))
            | TInt ->
                located_error ~loc
                  "Field access on %s expected boolean but found integer \
                   return value"
                  x
            | TReceipt _ -> failwith "Unreachable")
        | Some (TReceipt { ret_type; qos_types }), Successful ->
            Ok (T.BAccess (x, T.BSuccessful))
        | Some (TReceipt { ret_type; qos_types }), QosField f -> (
            match StringMap.find_opt f qos_types with
            | Some TBool -> Ok (T.BAccess (x, T.BQosField f))
            | Some TInt ->
                located_error ~loc
                  "Field access on %s expected boolean but found integer \
                   qos_types field %s"
                  x f
            | Some (TReceipt _) -> failwith "Unreachable"
            | None ->
                located_error ~loc
                  "Field access on %s expected boolean but qos_types field %s \
                   not found"
                  x f)
        | Some TInt, _ ->
            located_error ~loc
              "Variable %s expected receipt but found arithmetic" x
        | Some TBool, _ ->
            located_error ~loc "Variable %s expected receipt but found boolean"
              x
        | None, _ -> located_error ~loc "Variable %s not found" x)
    | EBoolNonDet -> Ok T.BNonDet
    | EIntNonDet ->
        located_error ~loc
          "Expected boolean expression but found arithmetic nondet"
    | EUnOp (Not, e) ->
        let* typed_e = type_check_bool ~scope ~fun_env e in
        Ok (T.BNot typed_e)
    | EBinOp (e1, op, e2) -> (
        match op with
        | And | Or -> (
            let* typed_e1 = type_check_bool ~scope ~fun_env e1 in
            let* typed_e2 = type_check_bool ~scope ~fun_env e2 in
            match op with
            | And -> Ok (T.BBoolOp (typed_e1, T.And, typed_e2))
            | Or -> Ok (T.BBoolOp (typed_e1, T.Or, typed_e2))
            | _ -> failwith "Unreachable")
        | Eq | Neq | Lt | Le | Gt | Ge -> (
            let* typed_e1 = type_check_arithm ~scope ~fun_env e1 in
            let* typed_e2 = type_check_arithm ~scope ~fun_env e2 in
            match op with
            | Eq -> Ok (T.BCmpOp (typed_e1, T.Eq, typed_e2))
            | Neq -> Ok (T.BCmpOp (typed_e1, T.Neq, typed_e2))
            | Lt -> Ok (T.BCmpOp (typed_e1, T.Lt, typed_e2))
            | Le -> Ok (T.BCmpOp (typed_e1, T.Le, typed_e2))
            | Gt -> Ok (T.BCmpOp (typed_e1, T.Gt, typed_e2))
            | Ge -> Ok (T.BCmpOp (typed_e1, T.Ge, typed_e2))
            | _ -> failwith "Unreachable")
        | Add | Sub | Mul | Div ->
            located_error ~loc
              "Expected boolean expression but found arithmetic expression")
    | EApp (f, args) -> (
        match StringMap.find_opt f fun_env with
        | Some (TFun (_, TBool)) ->
            let* args = type_check_app ~scope ~fun_env ~loc f args in
            Ok (T.BApp (f, args))
        | Some (TFun (_, TInt)) ->
            located_error ~loc
              "Function %s returns arithmetic but boolean expected" f
        | Some (TFun (_, TReceipt _)) -> failwith "Unreachable"
        | None -> located_error ~loc "Function %s not found" f)
  in
  Ok (located typed_e ~loc)

and type_check_expr ~scope ~fun_env t e =
  match t with
  | TInt -> (
      match type_check_arithm ~scope ~fun_env e with
      | Ok aexpr -> Ok (T.AExpr aexpr)
      | Error err -> Error err)
  | TBool -> (
      match type_check_bool ~scope ~fun_env e with
      | Ok bexpr -> Ok (T.BExpr bexpr)
      | Error err -> Error err)
  | TReceipt _ -> failwith "Unreachable"

and type_check_app ~scope ~fun_env ~loc f args =
  let* params_types =
    match StringMap.find_opt f fun_env with
    | Some (TFun (params_types, _)) -> Ok params_types
    | None -> located_error ~loc "Function %s not found" f
  in
  if List.length params_types <> List.length args then
    located_error ~loc "Function %s expected %d arguments but found %d" f
      (List.length params_types) (List.length args)
  else List.map2 (type_check_expr ~scope ~fun_env) params_types args |> all_ok

let rec has_fun_app e =
  match e.it with
  | EBool _ | EInt _ | EVar _ | EAccess _ | EBoolNonDet | EIntNonDet -> None
  | EUnOp (Not, e) -> has_fun_app e
  | EBinOp (e1, _, e2) -> (
      match has_fun_app e1 with Some loc -> Some loc | None -> has_fun_app e2)
  | EApp _ -> Some e.at
