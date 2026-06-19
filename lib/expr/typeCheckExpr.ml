open ExprAST
module T = TypedExprAST
open Utils.Data
open Utils.Types

let ( let* ) = Result.bind

let rec type_check_arithm scope static_fun_map = function
  | EInt n -> Ok (T.ALit n)
  | EBool _ -> Error "Expected arithmetic expression but found boolean literal"
  | EVar v -> (
      match lookup v scope with
      | Some TInt -> Ok (T.AVar v)
      | Some TBool ->
          Error (Fmt.str "Variable %s expected arithmetic but found boolean" v)
      | Some (TReceipt _) ->
          Error (Fmt.str "Variable %s expected arithmetic but found receipt" v)
      | None -> Error (Fmt.str "Arithmetic variable %s not found" v))
  | EAccess (x, field) -> (
      match (lookup x scope, field) with
      | Some (TReceipt { ret_type; qos_types }), ReturnValue -> (
          match ret_type with
          | TInt -> Ok (T.AAccess (x, T.AReturnValue))
          | TBool ->
              Error
                (Fmt.str
                   "Field access on %s expected arithmetic but found boolean \
                    return value"
                   x)
          | TReceipt _ -> failwith "Unreachable")
      | Some (TReceipt { ret_type; qos_types }), Successful ->
          Error
            (Fmt.str
               "Field access on %s expected arithmetic but found successful \
                field access"
               x)
      | Some (TReceipt { ret_type; qos_types }), QosField f -> (
          match StringMap.find_opt f qos_types with
          | Some TInt -> Ok (T.AAccess (x, T.AQosField f))
          | Some TBool ->
              Error
                (Fmt.str
                   "Field access on %s expected arithmetic but found boolean \
                    qos_types field %s"
                   x f)
          | Some (TReceipt _) -> failwith "Unreachable"
          | None ->
              Error
                (Fmt.str
                   "Field access on %s expected arithmetic but qos_types field \
                    %s not found"
                   x f))
      | Some TInt, _ ->
          Error (Fmt.str "Variable %s expected receipt but found arithmetic" x)
      | Some TBool, _ ->
          Error (Fmt.str "Variable %s expected receipt but found boolean" x)
      | None, _ -> Error (Fmt.str "Variable %s not found" x))
  | EIntNonDet -> Ok T.ANonDet
  | EBoolNonDet ->
      Error "Expected arithmetic expression but found boolean nondet"
  | EBinOp (e1, op, e2) -> (
      let* typed_e1 = type_check_arithm scope static_fun_map e1 in
      let* typed_e2 = type_check_arithm scope static_fun_map e2 in
      match op with
      | Add -> Ok (T.AOp (typed_e1, T.Add, typed_e2))
      | Sub -> Ok (T.AOp (typed_e1, T.Sub, typed_e2))
      | Mul -> Ok (T.AOp (typed_e1, T.Mul, typed_e2))
      | Div -> Ok (T.AOp (typed_e1, T.Div, typed_e2))
      | And | Or | Eq | Neq | Lt | Le | Gt | Ge ->
          Error "Expected arithmetic expression but found boolean operator")
  | EUnOp (Not, _) ->
      Error "Expected arithmetic expression but found unary boolean operator"
  | EApp (f, args) -> (
      match StringMap.find_opt f static_fun_map with
      | Some (TFun (params_types, TInt)) ->
          let* args =
            type_check_args f params_types args scope static_fun_map
          in
          Ok (T.AApp (f, args))
      | Some (TFun (_, TBool)) ->
          Error
            (Fmt.str "Function %s returns boolean but arithmetic expected" f)
      | Some (TFun (_, TReceipt _)) -> failwith "Unreachable"
      | None -> Error (Fmt.str "Function %s not found" f))

and type_check_bool scope static_fun_map = function
  | EBool b -> Ok (T.BLit b)
  | EInt _ -> Error "Expected boolean expression but found arithmetic literal"
  | EVar v -> (
      match lookup v scope with
      | Some TBool -> Ok (T.BVar v)
      | Some TInt ->
          Error (Fmt.str "Variable %s expected boolean but found arithmetic" v)
      | Some (TReceipt _) ->
          Error (Fmt.str "Variable %s expected boolean but found receipt" v)
      | None -> Error (Fmt.str "Boolean variable %s not found" v))
  | EAccess (x, field) -> (
      match (lookup x scope, field) with
      | Some (TReceipt { ret_type; qos_types }), ReturnValue -> (
          match ret_type with
          | TBool -> Ok (T.BAccess (x, T.BReturnValue))
          | TInt ->
              Error
                (Fmt.str
                   "Field access on %s expected boolean but found integer \
                    return value"
                   x)
          | TReceipt _ -> failwith "Unreachable")
      | Some (TReceipt { ret_type; qos_types }), Successful ->
          Ok (T.BAccess (x, T.BSuccessful))
      | Some (TReceipt { ret_type; qos_types }), QosField f -> (
          match StringMap.find_opt f qos_types with
          | Some TBool -> Ok (T.BAccess (x, T.BQosField f))
          | Some TInt ->
              Error
                (Fmt.str
                   "Field access on %s expected boolean but found integer \
                    qos_types field %s"
                   x f)
          | Some (TReceipt _) -> failwith "Unreachable"
          | None ->
              Error
                (Fmt.str
                   "Field access on %s expected boolean but qos_types field %s \
                    not found"
                   x f))
      | Some TInt, _ ->
          Error (Fmt.str "Variable %s expected receipt but found arithmetic" x)
      | Some TBool, _ ->
          Error (Fmt.str "Variable %s expected receipt but found boolean" x)
      | None, _ -> Error (Fmt.str "Variable %s not found" x))
  | EBoolNonDet -> Ok T.BNonDet
  | EIntNonDet ->
      Error "Expected boolean expression but found arithmetic nondet"
  | EUnOp (Not, e) ->
      let* typed_e = type_check_bool scope static_fun_map e in
      Ok (T.BNot typed_e)
  | EBinOp (e1, op, e2) -> (
      match op with
      | And | Or -> (
          let* typed_e1 = type_check_bool scope static_fun_map e1 in
          let* typed_e2 = type_check_bool scope static_fun_map e2 in
          match op with
          | And -> Ok (T.BBoolOp (typed_e1, T.And, typed_e2))
          | Or -> Ok (T.BBoolOp (typed_e1, T.Or, typed_e2))
          | _ -> failwith "Unreachable")
      | Eq | Neq | Lt | Le | Gt | Ge -> (
          let* typed_e1 = type_check_arithm scope static_fun_map e1 in
          let* typed_e2 = type_check_arithm scope static_fun_map e2 in
          match op with
          | Eq -> Ok (T.BCmpOp (typed_e1, T.Eq, typed_e2))
          | Neq -> Ok (T.BCmpOp (typed_e1, T.Neq, typed_e2))
          | Lt -> Ok (T.BCmpOp (typed_e1, T.Lt, typed_e2))
          | Le -> Ok (T.BCmpOp (typed_e1, T.Le, typed_e2))
          | Gt -> Ok (T.BCmpOp (typed_e1, T.Gt, typed_e2))
          | Ge -> Ok (T.BCmpOp (typed_e1, T.Ge, typed_e2))
          | _ -> failwith "Unreachable")
      | Add | Sub | Mul | Div ->
          Error "Expected boolean expression but found arithmetic expression")
  | EApp (f, args) -> (
      match StringMap.find_opt f static_fun_map with
      | Some (TFun (params_types, TBool)) ->
          let* args =
            type_check_args f params_types args scope static_fun_map
          in
          Ok (T.BApp (f, args))
      | Some (TFun (_, TInt)) ->
          Error
            (Fmt.str "Function %s returns arithmetic but boolean expected" f)
      | Some (TFun (_, TReceipt _)) -> failwith "Unreachable"
      | None -> Error (Fmt.str "Function %s not found" f))

and type_check_expr t scope static_fun_map e =
  match t with
  | TInt -> (
      match type_check_arithm scope static_fun_map e with
      | Ok aexpr -> Ok (T.AExpr aexpr)
      | Error err -> Error err)
  | TBool -> (
      match type_check_bool scope static_fun_map e with
      | Ok bexpr -> Ok (T.BExpr bexpr)
      | Error err -> Error err)
  | TReceipt _ -> failwith "Unreachable"

and type_check_args f params_types args scope static_fun_map =
  if List.length params_types <> List.length args then
    Error
      (Fmt.str "Function %s expected %d arguments but found %d" f
         (List.length params_types) (List.length args))
  else
    sequence_results
      (List.map2
         (fun t e -> type_check_expr t scope static_fun_map e)
         params_types args)
