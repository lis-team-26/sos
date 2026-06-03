open OrchestratorAST
open Symbolic.Data
open Symbolic.Runtime
open SymbolicMonad.MonadUtils
open Expr.AST
open Contract.AST
open Utils.Data
open FunctionalMonad
open OkStateMonad
open FunctionalMonad.Syntax
open OkStateMonad.Syntax

(* Extended state that carries policy checkers alongside the core monad state *)
type extended_state = {
  core : ok_monad_state;
  policy_checkers : PolicyChecker.pChecker list;
}

let total_env state =
  StringMap.union (fun _ priv _ -> Some priv) state.private_env state.public_env

let build_error msg (ok : ok_monad_state) = { msg; stack = ok.stack }
let init_error msg = Symex.Result.error (build_error msg)
let raise_error msg ok = Symex.Result.error (build_error msg ok)

let cast_to_int v =
  match Typed.cast_checked v Typed.t_int with
  | Some i -> Symex.Result.ok i
  | None -> init_error "Expected an integer"

let cast_to_bool v =
  match Typed.cast_checked v Typed.t_bool with
  | Some b -> Symex.Result.ok b
  | None -> init_error "Expected a boolean"

let checked_update_env t x v =
  let& state = get in
  let&* v =
    (match t with
      | TInt ->
          let++ v = cast_to_int v in
          SymbInt v
      | TBool ->
          let++ v = cast_to_bool v in
          SymbBool v)
    |> seal_error state
  in
  put { state with private_env = StringMap.add x v state.private_env }

let symb_eval_arithm_op v1 op v2 =
  let** v1 = cast_to_int v1 in
  let** v2 = cast_to_int v2 in
  match op with
  | Add -> Symex.Result.ok (v1 +@ v2)
  | Sub -> Symex.Result.ok (v1 -@ v2)
  | Mul -> Symex.Result.ok (v1 *@ v2)
  | Div ->
      if%sat v2 ==@ Typed.int 0 then init_error "Division by zero"
      else
        let v2 = Typed.cast v2 in
        Symex.Result.ok (v1 /@ v2)
  | _ -> init_error "Type error in arithmetic operation"

let symb_eval_bool_bin_op v1 op v2 =
  let** v1 = cast_to_bool v1 in
  let** v2 = cast_to_bool v2 in
  match op with
  | And -> Symex.Result.ok (v1 &&@ v2)
  | Or -> Symex.Result.ok (v1 ||@ v2)
  | _ -> init_error "Type error in boolean operation"

let symb_eval_bool_un_op op v =
  let++ v = cast_to_bool v in
  match op with Not -> Typed.not v

let symb_eval_cmp_op v1 op v2 =
  let** v1 = cast_to_int v1 in
  let** v2 = cast_to_int v2 in
  match op with
  | Lt -> Symex.Result.ok (v1 <@ v2)
  | Le -> Symex.Result.ok (v1 <=@ v2)
  | Gt -> Symex.Result.ok (v1 >@ v2)
  | Ge -> Symex.Result.ok (v1 >=@ v2)
  | Eq -> Symex.Result.ok (v1 ==@ v2)
  | Neq -> Symex.Result.ok (Typed.not (v1 ==@ v2))
  | _ -> init_error "Type error in comparison operation"

let rec symb_eval_expr env = function
  | EInt n -> return (Typed.int n)
  | EBool b -> return (Typed.of_bool b)
  | EVar x ->
      let&&* value =
        match StringMap.find_opt x env with
        | Some (SymbInt i) -> Symex.Result.ok (Typed.cast i)
        | Some (SymbBool b) -> Symex.Result.ok (Typed.cast b)
        | None -> (
            (* Check if it's an uninterpreted constant in function_map *)
            match StringMap.find_opt x function_map with
            | Some (TFun ([], ret_type), fun_invoke) -> (
                let* _, v_opt = SymbolicListMap.find_opt [] fun_invoke in
                match v_opt with
                | Some (SymbInt i) -> Symex.Result.ok (Typed.cast i)
                | Some (SymbBool b) -> Symex.Result.ok (Typed.cast b)
                | None ->
                    (* Create fresh value *)
                    let* v = Symex.nondet Typed.t_int in
                    (* let symb_val = SymbInt (Typed.cast v) in *)
                    (* let new_invoke = SymbolicListMap.syntactic_add [] symb_val fun_invoke in *)
                    let _ = (* update function_map via side channel won't work in FSM... *)
                      () in
                    Symex.Result.ok (Typed.cast v))
            | _ ->
                (* Treat as fresh uninterpreted constant *)
                let* v = Symex.nondet Typed.t_int in
                Symex.Result.ok (Typed.cast v))
      in
      value
  | ENonDet ->
      let&&+ v = Symex.nondet Typed.t_int in
      Typed.cast v
  | EApp (f, args) -> (
      let& function_map = get in
      match StringMap.find_opt f function_map with
      | None -> failwith (Fmt.str "Function %s not found" f)
      | Some (TFun (typed_vars, ret_type), _) -> (
          let typed_args = List.combine typed_vars args in
          let& actual_args =
            map_list typed_args ~f:(fun (t, x) ->
                match t with
                | TInt ->
                    let& v = symb_eval_aexpr env x in
                    return (Typed.cast v)
                | TBool ->
                    let& v = symb_eval_bexpr env x in
                    return (Typed.cast v))
          in
          let& function_map = get in
          match StringMap.find_opt f function_map with
          | None -> failwith (Fmt.str "Function %s not found" f)
          | Some (TFun (_, _), fun_invoke) -> (
              let&+ _, return_value_opt =
                SymbolicListMap.find_opt actual_args fun_invoke
              in
              match return_value_opt with
              | Some (SymbInt i) -> return (Typed.cast i)
              | Some (SymbBool b) -> return (Typed.cast b)
              | None ->
                  let&+ return_value =
                    match ret_type with
                    | TInt -> Symex.nondet Typed.t_int
                    | TBool -> Symex.nondet Typed.t_bool
                  in
                  let symb_ret_value =
                    match ret_type with
                    | TInt -> SymbInt (Typed.cast return_value)
                    | TBool -> SymbBool (Typed.cast return_value)
                  in
                  let new_fun_invoke =
                    SymbolicListMap.syntactic_add actual_args symb_ret_value
                      fun_invoke
                  in
                  let& () =
                    modify
                      (StringMap.add f
                         (TFun (typed_vars, ret_type), new_fun_invoke))
                  in
                  return return_value)))
  | EUnOp (op, e) ->
      let& v = symb_eval_expr env e in
      let&&* v = symb_eval_bool_un_op op v in
      Typed.cast v
  | EBinOp (e1, op, e2) ->
      let& v1 = symb_eval_expr env e1 in
      let& v2 = symb_eval_expr env e2 in
      let&&* result =
        match op with
        | Add | Sub | Mul | Div -> symb_eval_arithm_op v1 op v2
        | And | Or -> symb_eval_bool_bin_op v1 op v2
        | Lt | Le | Gt | Ge | Eq | Neq ->
            let++ comp_result = symb_eval_cmp_op v1 op v2 in
            Typed.cast comp_result
      in
      Typed.cast result

and symb_eval_aexpr env e =
  let& v = symb_eval_expr env e in
  let&&* i = cast_to_int v in
  i

and symb_eval_bexpr env e =
  let& v = symb_eval_expr env e in
  let&&* b = cast_to_bool v in
  b

let rec symb_eval_stmt stmt =
  let& state = get in
  match stmt with
  | Seq (s1, s2) ->
      let& () = symb_eval_stmt s1 in
      symb_eval_stmt s2
  | If (e, then_s, else_s) ->
      let& b = lift_fm (symb_eval_bexpr (total_env state) e) in
      branch b (symb_eval_stmt then_s) (symb_eval_stmt else_s)
  | While (e, s) ->
      let& b = lift_fm (symb_eval_bexpr (total_env state) e) in
      branch b
        (let& () = symb_eval_stmt s in
         symb_eval_stmt (While (e, s)))
        (return ())
  | stmt -> symb_eval_simple_stmt stmt

and symb_eval_simple_stmt stmt =
  let& state = get in
  match stmt with
  | Skip -> return ()
  | Declare (t, x, e) -> (
      let state = ext.core in
      match t with
      | TInt ->
          let& v = lift_fm (symb_eval_aexpr (total_env state) e) in
          checked_update_env t x v
      | TBool ->
          let& v = lift_fm (symb_eval_bexpr (total_env state) e) in
          checked_update_env t x v)
  | Assign (x, e) ->
      let&* t =
        (match StringMap.find_opt x state.private_env with
          | Some (SymbInt _) -> Symex.Result.ok TInt
          | Some (SymbBool _) -> Symex.Result.ok TBool
          | None -> init_error (Fmt.str "Variable %s not declared" x))
        |> seal_error state
      in
      let& v =
        lift_fm
          (match t with
          | TInt -> symb_eval_aexpr (total_env state) e
          | TBool -> symb_eval_bexpr (total_env state) e)
      in
      (* Is it really necessay a checked insert? *)
      checked_update_env t x v
  | Assume e ->
      let& b = lift_fm (symb_eval_bexpr (total_env state) e) in
      let&+ () = Symex.assume [ b ] in
      return ()
  | Assert e ->
      let& b = lift_fm (symb_eval_bexpr (total_env state) e) in
      let&* () =
        Symex.assert_or_error b
          (build_error (Fmt.str "Assertion failed: %a" Typed.ppa b) state)
      in
      return ()
  | Invoke (f, args) ->
      let&* service =
        (match StringMap.find_opt f state.service_map with
          | Some s -> Symex.Result.ok s
          | None -> init_error (Fmt.str "Service %s not found" f))
        |> seal_error state
      in
      let typed_args = List.combine service.params args in
      let& args_env =
        lift_fm
          (fold_list typed_args ~init:StringMap.empty
             ~f:(fun acc_args_env ((x, t), e) ->
               match t with
               | TInt ->
                   let& arg = symb_eval_aexpr (total_env state) e in
                   return (StringMap.add x (SymbInt arg) acc_args_env)
               | TBool ->
                   let& arg = symb_eval_bexpr (total_env state) e in
                   return (StringMap.add x (SymbBool arg) acc_args_env)))
      in
      let precond_env =
        (* The environment for evaluating preconditions gives precedence to the arguments *)
        StringMap.union (fun _ arg _ -> Some arg) args_env state.public_env
      in
      let& b =
        lift_fm
          (fold_list service.precond ~init:Typed.v_true ~f:(fun acc_precond e ->
               let& b = symb_eval_bexpr precond_env e in
               return (acc_precond &&@ b)))
      in
      let&* () =
        Symex.assert_or_error b
          (build_error
             (Fmt.str "Precondition violated for service '%s'" f)
             state)
      in
      let&+ success =
        if Option.is_none service.err_postcond then Symex.return Typed.v_true
        else Symex.nondet Typed.t_bool
      in
      failwith "Not yet implemented"
  | DeclareInvoke (x, t, f, args) -> failwith "Not yet implemented"
  | AssignInvoke (x, serv, args) ->
      (* let** args =
        Symex.Result.map_list args ~f:(fun arg ->
            wrap_error (symb_eval_aexpr s.env arg) state.stack)
      in
      let* cost = Symex.nondet Typed.t_int in
      let* latency = Symex.nondet Typed.t_int in
      let* ret_val = Symex.nondet Typed.t_int in
      let call = { serv_name = serv; args; qos = { cost; latency } } in
      Symex.Result.ok
        { env = StringMap.add x ret_val state.env; stack = call :: state.stack } *)
      failwith "Not yet implemented"
  | _ as stmt -> symb_eval_simple_stmt stmt

(* contract is only needed by build_symb_process, not to evaluate statements *)
let build_symb_process stmt contract policy_init_states =
  let private_env = StringMap.empty in
  let* public_env =
    Symex.fold_list contract.globals ~init:StringMap.empty ~f:(fun acc (x, t) ->
        let* v =
          match t with
          | TInt ->
              let* v = Symex.nondet Typed.t_int in
              Symex.return (SymbInt v)
          | TBool ->
              let* v = Symex.nondet Typed.t_bool in
              Symex.return (SymbBool v)
        in
        Symex.return (StringMap.add x v acc))
  in
  let service_map =
    List.fold_left
      (fun m s -> StringMap.add s.name s m)
      StringMap.empty contract.services
  in
  let function_map =
    List.fold_left
      (fun m (f, t) -> StringMap.add f (t, SymbolicListMap.empty) m)
      StringMap.empty contract.functions
  in
  let core =
    { private_env; public_env; service_map; function_map; stack = [] }
  in
  let symbolic_results = run_unit (symb_eval_stmt stmt) state in
  symbolic_results
  |> Fun.flip Symex.Result.map (fun (s : ok_monad_state) ->
      { s with stack = List.rev s.stack })
  |> Fun.flip Symex.Result.map_error (fun (s : err_monad_state) ->
      { s with stack = List.rev s.stack })
