open Expr.AST
open TypedExpr.AST
open Contract.AST
open Utils.Data
open Format

type static_scope = var_type scope_stack

let rec type_check_arithm scope static_fn_map = function
  | EInt n -> Ok (ALit n)
  | EBool _ -> Error "Expected numerical expression but found boolean literal"
  | EVar v -> (
      match lookup v scope with
      | Some TInt -> Ok (AVar v)
      | Some TBool -> Error (sprintf "Variable %s expected numerical but found boolean" v)
      | None -> Error (sprintf "Numerical variable %s not found" v))
  | EIntNonDet -> Ok ANonDet
  | EBoolNonDet -> Error "Expected numerical expression but found boolean nondet"
  | EBinOp (e1, op, e2) -> (
      match (type_check_arithm scope static_fn_map e1, type_check_arithm scope static_fn_map e2) with
      | Ok v1, Ok v2 -> (
          match op with
          | Add -> Ok (AOp (v1, TAdd, v2))
          | Sub -> Ok (AOp (v1, TSub, v2))
          | Mul -> Ok (AOp (v1, TMul, v2))
          | Div -> Ok (AOp (v1, TDiv, v2))
          | _ -> Error "Expected arithmetic operator")
      | Error err, _ -> Error err
      | _, Error err -> Error err)
  | EUnOp _ -> Error "Expected numerical expression but found unary operation"
  | EApp (f, args) -> (
      match StringMap.find_opt f static_fn_map with
      | Some (TFun (params_types, TInt)) ->
          if List.length params_types <> List.length args then
            Error (sprintf "Function %s expected %d arguments but found %d" f (List.length params_types) (List.length args))
          else
            let type_checked_args =
              List.map2 (fun t e -> type_check_expr_as t scope static_fn_map e) params_types args
            in
            let rec check_all = function
              | [] -> Ok []
              | Ok e :: rest -> (
                  match check_all rest with
                  | Ok rest' -> Ok (e :: rest')
                  | Error err -> Error err)
              | Error err :: _ -> Error err
            in
            (match check_all type_checked_args with
            | Ok args' -> Ok (AApp (f, args'))
            | Error err -> Error err)
      | Some (TFun (_, TBool)) -> Error (sprintf "Function %s returns boolean but numerical expected" f)
      | None -> Error (sprintf "Function %s not found" f))

and type_check_bool scope static_fn_map = function
  | EBool b -> Ok (BLit b)
  | EInt _ -> Error "Expected boolean expression but found numerical literal"
  | EVar v -> (
      match lookup v scope with
      | Some TBool -> Ok (BVar v)
      | Some TInt -> Error (sprintf "Variable %s expected boolean but found numerical" v)
      | None -> Error (sprintf "Boolean variable %s not found" v))
  | EBoolNonDet -> Ok BNonDet
  | EIntNonDet -> Error "Expected boolean expression but found numerical nondet"
  | EUnOp (Not, e) -> (
      match type_check_bool scope static_fn_map e with
      | Ok v -> Ok (BNot v)
      | Error err -> Error err)
  | EBinOp (e1, op, e2) -> (
      match op with
      | And | Or -> (
          match (type_check_bool scope static_fn_map e1, type_check_bool scope static_fn_map e2) with
          | Ok v1, Ok v2 -> (
              match op with
              | And -> Ok (BBoolOp (v1, TAnd, v2))
              | Or -> Ok (BBoolOp (v1, TOr, v2))
              | _ -> assert false)
          | Error err, _ -> Error err
          | _, Error err -> Error err)
      | Eq | Neq | Lt | Le | Gt | Ge -> (
          match (type_check_arithm scope static_fn_map e1, type_check_arithm scope static_fn_map e2) with
          | Ok v1, Ok v2 -> (
              match op with
              | Eq -> Ok (BCmpOp (v1, TEq, v2))
              | Neq -> Ok (BCmpOp (v1, TNeq, v2))
              | Lt -> Ok (BCmpOp (v1, TLt, v2))
              | Le -> Ok (BCmpOp (v1, TLe, v2))
              | Gt -> Ok (BCmpOp (v1, TGt, v2))
              | Ge -> Ok (BCmpOp (v1, TGe, v2))
              | _ -> assert false)
          | Error err, _ -> Error err
          | _, Error err -> Error err)
      | _ -> Error "Expected boolean operator or comparison operator")
  | EApp (f, args) -> (
      match StringMap.find_opt f static_fn_map with
      | Some (TFun (params_types, TBool)) ->
          if List.length params_types <> List.length args then
            Error (sprintf "Function %s expected %d arguments but found %d" f (List.length params_types) (List.length args))
          else
            let type_checked_args =
              List.map2 (fun t e -> type_check_expr_as t scope static_fn_map e) params_types args
            in
            let rec check_all = function
              | [] -> Ok []
              | Ok e :: rest -> (
                  match check_all rest with
                  | Ok rest' -> Ok (e :: rest')
                  | Error err -> Error err)
              | Error err :: _ -> Error err
            in
            (match check_all type_checked_args with
            | Ok args' -> Ok (BApp (f, args'))
            | Error err -> Error err)
      | Some (TFun (_, TInt)) -> Error (sprintf "Function %s returns numerical but boolean expected" f)
      | None -> Error (sprintf "Function %s not found" f))

and type_check_expr_as t scope static_fn_map e =
  match t with
  | TInt -> (
      match type_check_arithm scope static_fn_map e with
      | Ok aexpr -> Ok (AExpr aexpr)
      | Error err -> Error err)
  | TBool -> (
      match type_check_bool scope static_fn_map e with
      | Ok bexpr -> Ok (BExpr bexpr)
      | Error err -> Error err)

let type_check scope static_fn_map e =
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
      | Some TInt -> Ok (AExpr (AVar v))
      | Some TBool -> Ok (BExpr (BVar v))
      | None -> Error (sprintf "Variable %s not found" v))
  | EBinOp (_, op, _) -> (
      match op with
      | Add | Sub | Mul | Div -> (
          match type_check_arithm scope static_fn_map e with
          | Ok a -> Ok (AExpr a)
          | Error err -> Error err)
      | And | Or | Eq | Neq | Lt | Le | Gt | Ge -> (
          match type_check_bool scope static_fn_map e with
          | Ok b -> Ok (BExpr b)
          | Error err -> Error err))
  | EApp (f, _) -> (
      match StringMap.find_opt f static_fn_map with
      | Some (TFun (_, TInt)) -> (
          match type_check_arithm scope static_fn_map e with
          | Ok a -> Ok (AExpr a)
          | Error err -> Error err)
      | Some (TFun (_, TBool)) -> (
          match type_check_bool scope static_fn_map e with
          | Ok b -> Ok (BExpr b)
          | Error err -> Error err)
      | None -> Error (sprintf "Function %s not found" f))
