open Expr.TypedAST
open Symbolic.Data
open Symbolic.Runtime
open StateMonad.FunctionalMonad
open Utils.Data
open Utils.Types

let symb_eval_arithm_op v1 op v2 =
  match op with
  | Add -> Symex.Result.ok (v1 +@ v2)
  | Sub -> Symex.Result.ok (v1 -@ v2)
  | Mul -> Symex.Result.ok (v1 *@ v2)
  | Div ->
      if%sat v2 ==@ Typed.int 0 then Symex.Result.error "Division by zero"
      else
        let v2 = Typed.cast v2 in
        Symex.Result.ok (v1 /@ v2)

let symb_eval_bool_bin_op v1 op v2 =
  match op with
  | And -> Symex.Result.ok (v1 &&@ v2)
  | Or -> Symex.Result.ok (v1 ||@ v2)

let symb_eval_cmp_op v1 op v2 =
  match op with
  | Lt -> Symex.Result.ok (v1 <@ v2)
  | Le -> Symex.Result.ok (v1 <=@ v2)
  | Gt -> Symex.Result.ok (v1 >@ v2)
  | Ge -> Symex.Result.ok (v1 >=@ v2)
  | Eq -> Symex.Result.ok (v1 ==@ v2)
  | Neq -> Symex.Result.ok (Typed.not (v1 ==@ v2))

let rec symb_eval_aexpr env = function
  | ALit n -> return (Typed.int n)
  | AVar x ->
      let value =
        match lookup x env with
        | Some (SymbInt i) -> i
        | _ ->
            failwith (Fmt.str "Unreachable: expected arithmetic variable %s" x)
      in
      return value
  | AAccess (x, field) -> (
      match (lookup x env, field) with
      | Some (SymbReceipt { ret_val; successful; qos_fields }), AReturnValue ->
          return (cast ret_val)
      | Some (SymbReceipt { ret_val; successful; qos_fields }), AQosField f -> (
          match lookup f [ qos_fields ] with
          | Some (SymbInt v) -> return v
          | _ -> failwith "Unreachable: QoS field not found or not an int")
      | _ -> failwith "Unreachable: expected receipt variable for field access")
  | ANonDet ->
      let&+ v = Symex.nondet Typed.t_int in
      v
  | AOp (e1, op, e2) ->
      let& v1 = symb_eval_aexpr env e1 in
      let& v2 = symb_eval_aexpr env e2 in
      let&** result = symb_eval_arithm_op v1 op v2 in
      return result
  | AApp (f, args) -> (
      let& v = symb_eval_app env TInt f args in
      match v with
      | SymbInt v -> return v
      | _ ->
          failwith
            "Unreachable: expected arithmetic return value from function \
             application")

and symb_eval_bexpr env = function
  | BLit b -> return (Typed.of_bool b)
  | BVar x ->
      let value =
        match lookup x env with
        | Some (SymbBool b) -> b
        | _ -> failwith "Unreachable: expected boolean variable"
      in
      return value
  | BAccess (x, field) -> (
      match (lookup x env, field) with
      | Some (SymbReceipt { ret_val; successful; qos_fields }), BReturnValue ->
          return (cast ret_val)
      | Some (SymbReceipt { ret_val; successful; qos_fields }), BSuccessful ->
          return successful
      | Some (SymbReceipt { ret_val; successful; qos_fields }), BQosField f -> (
          match lookup f [ qos_fields ] with
          | Some (SymbBool v) -> return v
          | _ -> failwith "Unreachable: QoS field not found or not a bool")
      | _ -> failwith "Unreachable: expected receipt variable for field access")
  | BNonDet ->
      let&+ v = Symex.nondet Typed.t_bool in
      v
  | BBoolOp (e1, op, e2) ->
      let& v1 = symb_eval_bexpr env e1 in
      let& v2 = symb_eval_bexpr env e2 in
      let&** result = symb_eval_bool_bin_op v1 op v2 in
      return result
  | BCmpOp (e1, op, e2) ->
      let& v1 = symb_eval_aexpr env e1 in
      let& v2 = symb_eval_aexpr env e2 in
      let&** result = symb_eval_cmp_op v1 op v2 in
      return result
  | BNot e ->
      let& v = symb_eval_bexpr env e in
      return (Typed.not v)
  | BApp (f, args) -> (
      let& v = symb_eval_app env TBool f args in
      match v with
      | SymbBool v -> return v
      | _ ->
          failwith
            "Unreachable: expected boolean return value from function \
             application")

and symb_eval_app env t f args =
  let& fun_envs = get in
  let fun_env =
    match StringMap.find_opt f fun_envs with
    | Some v -> v
    | None -> failwith "Unreachable: function not found in environment"
  in
  let& actual_args =
    map_list args ~f:(fun e ->
        match e with
        | AExpr e ->
            let& v = symb_eval_aexpr env e in
            return (Typed.cast v)
        | BExpr e ->
            let& v = symb_eval_bexpr env e in
            return (Typed.cast v))
  in
  let&* syntactic_key, opt_ret_val =
    SymbolicListMap.find_opt actual_args fun_env
  in
  match (opt_ret_val, t) with
  | Some (SymbInt v), TInt -> return (SymbInt v)
  | Some (SymbBool v), TBool -> return (SymbBool v)
  | None, TInt ->
      let&* ret_val = Symex.nondet Typed.t_int in
      let fun_env =
        SymbolicListMap.syntactic_add syntactic_key (SymbInt ret_val) fun_env
      in
      let& () = modify (StringMap.add f fun_env) in
      return (SymbInt ret_val)
  | None, TBool ->
      let&* ret_val = Symex.nondet Typed.t_bool in
      let fun_env =
        SymbolicListMap.syntactic_add syntactic_key (SymbBool ret_val) fun_env
      in
      let& () = modify (StringMap.add f fun_env) in
      return (SymbBool ret_val)
  | _ ->
      failwith "Unreachable: function return type does not match expected type"

let symb_eval_expr env = function
  | AExpr e ->
      let& v = symb_eval_aexpr env e in
      return (SymbInt v)
  | BExpr e ->
      let& v = symb_eval_bexpr env e in
      return (SymbBool v)
