open ExprAST
module T = TypedExprAST
open Utils.Data
open Utils.Types

let ( let* ) = Result.bind

let rec type_check_arithm scope static_fn_map = function
  | EInt n -> Ok (T.ALit n)
  | EBool _ -> Error "Expected arithmetic expression but found boolean literal"
  | EVar v -> (
      match lookup v scope with
      | Some TInt -> Ok (T.AVar v)
      | Some TBool ->
          Error (Fmt.str "Variable %s expected arithmetic but found boolean" v)
      | None -> Error (Fmt.str "Arithmetic variable %s not found" v))
  | EIntNonDet -> Ok T.ANonDet
  | EBoolNonDet ->
      Error "Expected arithmetic expression but found boolean nondet"
  | EBinOp (e1, op, e2) -> (
      let* typed_e1 = type_check_arithm scope static_fn_map e1 in
      let* typed_e2 = type_check_arithm scope static_fn_map e2 in
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
      match StringMap.find_opt f static_fn_map with
      | Some (TFun (params_types, TInt)) ->
          let* args = type_check_args f params_types args scope static_fn_map in
          Ok (T.AApp (f, args))
      | Some (TFun (_, TBool)) ->
          Error
            (Fmt.str "Function %s returns boolean but arithmetic expected" f)
      | None -> Error (Fmt.str "Function %s not found" f))

and type_check_bool scope static_fn_map = function
  | EBool b -> Ok (T.BLit b)
  | EInt _ -> Error "Expected boolean expression but found arithmetic literal"
  | EVar v -> (
      match lookup v scope with
      | Some TBool -> Ok (T.BVar v)
      | Some TInt ->
          Error (Fmt.str "Variable %s expected boolean but found arithmetic" v)
      | None -> Error (Fmt.str "Boolean variable %s not found" v))
  | EBoolNonDet -> Ok T.BNonDet
  | EIntNonDet ->
      Error "Expected boolean expression but found arithmetic nondet"
  | EUnOp (Not, e) ->
      let* typed_e = type_check_bool scope static_fn_map e in
      Ok (T.BNot typed_e)
  | EBinOp (e1, op, e2) -> (
      match op with
      | And | Or -> (
          let* typed_e1 = type_check_bool scope static_fn_map e1 in
          let* typed_e2 = type_check_bool scope static_fn_map e2 in
          match op with
          | And -> Ok (T.BBoolOp (typed_e1, T.And, typed_e2))
          | Or -> Ok (T.BBoolOp (typed_e1, T.Or, typed_e2))
          | _ -> failwith "Unreachable")
      | Eq | Neq | Lt | Le | Gt | Ge -> (
          let* typed_e1 = type_check_arithm scope static_fn_map e1 in
          let* typed_e2 = type_check_arithm scope static_fn_map e2 in
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
      match StringMap.find_opt f static_fn_map with
      | Some (TFun (params_types, TBool)) ->
          let* args = type_check_args f params_types args scope static_fn_map in
          Ok (T.BApp (f, args))
      | Some (TFun (_, TInt)) ->
          Error
            (Fmt.str "Function %s returns arithmetic but boolean expected" f)
      | None -> Error (Fmt.str "Function %s not found" f))

and type_check_expr t scope static_fn_map e =
  match t with
  | TInt -> (
      match type_check_arithm scope static_fn_map e with
      | Ok aexpr -> Ok (T.AExpr aexpr)
      | Error err -> Error err)
  | TBool -> (
      match type_check_bool scope static_fn_map e with
      | Ok bexpr -> Ok (T.BExpr bexpr)
      | Error err -> Error err)

and type_check_args f params_types args scope static_fn_map =
  if List.length params_types <> List.length args then
    Error
      (Fmt.str "Function %s expected %d arguments but found %d" f
         (List.length params_types) (List.length args))
  else
    sequence_results
      (List.map2
         (fun t e -> type_check_expr t scope static_fn_map e)
         params_types args)

(*let type_check scope static_fn_map e =
  match e with
  | EInt _ | EIntNonDet -> (
      match type_check_arithm scope static_fn_map e with
      | Ok a -> Ok (AExpr a)
      | Error err -> Error err)
  | EBool _ | EBoolNonDet | EUnOp _ -> (
      match type_check_bool scope static_fn_map e with
      | Ok b -> Ok (BExpr b)
      | Error err -> Error err)
  | EVar v -> (
      match lookup v scope with
      | Some TInt -> Ok (T.AExpr (T.AVar v))
      | Some TBool -> Ok (T.BExpr (T.BVar v))
      | None -> Error (Fmt.str "Variable %s not found" v))
  | EBinOp (_, op, _) -> (
      match op with
      | Add | Sub | Mul | Div -> (
          match type_check_arithm scope static_fn_map e with
          | Ok a -> Ok (T.AExpr a)
          | Error err -> Error err)
      | And | Or | Eq | Neq | Lt | Le | Gt | Ge -> (
          match type_check_bool scope static_fn_map e with
          | Ok b -> Ok (T.BExpr b)
          | Error err -> Error err))
  | EApp (f, _) -> (
      match StringMap.find_opt f static_fn_map with
      | Some (TFun (_, TInt)) -> (
          match type_check_arithm scope static_fn_map e with
          | Ok a -> Ok (T.AExpr a)
          | Error err -> Error err)
      | Some (TFun (_, TBool)) -> (
          match type_check_bool scope static_fn_map e with
          | Ok b -> Ok (T.BExpr b)
          | Error err -> Error err)
      | None -> Error (Fmt.str "Function %s not found" f))
*)
