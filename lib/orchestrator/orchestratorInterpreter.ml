open TypedOrchestratorAST
open Expr.TypedAST
open Contract.TypedAST
open ExprInterpreter
open Symbolic.Data
open Symbolic.Runtime
open StateMonad.OkStateMonad
open StateMonad.Utils
open PolicyChecker
open Utils.Data
open Utils.Types

let symb_eval_constraints scope constraints =
  fold_list constraints ~init:Typed.v_true ~f:(fun acc e ->
      let& b = lift_fm (symb_eval_bexpr scope e) in
      return (acc &&@ b))

let symb_eval_effects scope effects =
  fold_list effects ~init:scope ~f:(fun scope (lhs, rhs) ->
      match lhs with
      | LVar x ->
          let& v = lift_fm (symb_eval_expr scope rhs) in
          return (update x v scope)
      | LApp (f, args) ->
          let& actual_args =
            map_list args ~f:(fun e ->
                let& v = lift_fm (symb_eval_expr scope e) in
                return (cast v))
          in
          let& v = lift_fm (symb_eval_expr scope rhs) in
          let& state, policy_checkers = get in
          let fun_env =
            match StringMap.find_opt f state.function_envs with
            | Some v -> v
            | None -> failwith "Unreachable: function not found in environment"
          in
          let&* syntactic_key, _ =
            SymbolicListMap.find_opt actual_args fun_env
          in
          let fun_env = SymbolicListMap.syntactic_add actual_args v fun_env in
          let fun_envs = StringMap.add f fun_env state.function_envs in
          let& () =
            modify (fun (state, policy_checkers) ->
                ({ state with function_envs = fun_envs }, policy_checkers))
          in
          return scope)

let symb_eval_postcond postcond scope service =
  let effects, constraints = postcond in
  let& scope = symb_eval_effects scope effects in
  let& constraints = symb_eval_constraints scope constraints in
  let&* () = Symex.assume [ constraints ] in
  return scope

let symb_eval_invoke svc args qos_fields =
  let& state = get_state in
  let pre_invoke_scope = state.scope in
  let service =
    match StringMap.find_opt svc state.service_map with
    | Some svc -> svc
    | None -> failwith "Unreachable: service not found in environment"
  in
  let actual_args = List.combine service.params args in
  let& actual_args_env =
    fold_list actual_args ~init:StringMap.empty ~f:(fun env (x, e) ->
        match e with
        | AExpr e ->
            let& v = lift_fm (symb_eval_aexpr pre_invoke_scope e) in
            let v = SymbInt v in
            return (StringMap.add x v env)
        | BExpr e ->
            let& v = lift_fm (symb_eval_bexpr pre_invoke_scope e) in
            let v = SymbBool v in
            return (StringMap.add x v env))
  in
  let precond_scope = [ actual_args_env; get_public_env pre_invoke_scope ] in
  let& b =
    fold_list service.precond ~init:Typed.v_true ~f:(fun acc_precond e ->
        let& b = lift_fm (symb_eval_bexpr precond_scope e) in
        return (acc_precond &&@ b))
  in
  let&** () =
    Symex.assert_or_error b
      { cause = ServicePrecond svc; err_stack = state.ok_stack }
  in
  let& qos_env =
    fold_list qos_fields ~init:StringMap.empty ~f:(fun env (x, t) ->
        match t with
        | TInt ->
            let&* v = Symex.nondet Typed.t_int in
            let v = SymbInt v in
            return (StringMap.add x v env)
        | TBool ->
            let&* v = Symex.nondet Typed.t_bool in
            let v = SymbBool v in
            return (StringMap.add x v env)
        | TReceipt _ -> failwith "Unreachable: receipts cannot be QoS fields")
  in
  let& qos_scope =
    symb_eval_postcond service.qos_postcond (qos_env :: precond_scope) service
  in
  let qos_env = List.hd qos_scope in
  let&* ret_var, nondet_ret_val =
    match service.returns with
    | x, TInt ->
        let+ v = Symex.nondet Typed.t_int in
        (x, SymbInt v)
    | x, TBool ->
        let+ v = Symex.nondet Typed.t_bool in
        (x, SymbBool v)
    | _ -> failwith "Unreachable: service return type must be int or bool"
  in
  let postcond_scope =
    qos_scope |> pop_env |> push_env |> declare ret_var nondet_ret_val
  in
  let&* successful =
    match service.err_postcond with
    | None -> Symex.return Typed.v_true
    | Some _ -> Symex.nondet Typed.t_bool
  in
  let& state, policy_checkers = get in
  let&** postcond_scope, _ =
    if%sat successful then
      run
        (symb_eval_postcond service.ok_postcond postcond_scope service)
        (state, policy_checkers)
    else
      let err_postcond =
        match service.err_postcond with
        | Some err_postcond -> err_postcond
        | None ->
            failwith
              "Unreachable: if service has no error postcondition, it must be \
               successful"
      in
      run
        (symb_eval_postcond err_postcond postcond_scope service)
        (state, policy_checkers)
  in
  let post_invoke_scope =
    pre_invoke_scope |> set_public_env (get_public_env postcond_scope)
  in
  let ret_val =
    match lookup ret_var postcond_scope with
    | Some v -> v
    | None ->
        failwith "Unreachable: return variable not found in postcondition scope"
  in
  let invocation =
    {
      service;
      actual_args = actual_args_env;
      ret_val;
      successful;
      actual_qos = qos_env;
    }
  in
  let& () =
    modify_state (fun state ->
        {
          state with
          scope = post_invoke_scope;
          ok_stack = invocation :: state.ok_stack;
        })
  in
  let& state, policy_checkers = get in
  let& policy_checkers =
    map_list policy_checkers ~f:(fun pc ->
        let&** pc = update_policy invocation pc |> map_error state in
        return pc)
  in
  let& () = modify_policy_checkers (fun _ -> policy_checkers) in
  return (SymbReceipt { ret_val; successful; qos_fields = qos_env })

let rec symb_eval_stmt c stmt =
  let& state = get_state in
  match stmt with
  | Skip -> return ()
  | Declare (x, e) ->
      let& v =
        match e with
        | AExpr e ->
            let& v = symb_eval_aexpr state.scope e |> lift_fm in
            return (SymbInt v)
        | BExpr e ->
            let& v = symb_eval_bexpr state.scope e |> lift_fm in
            return (SymbBool v)
      in
      modify_state (fun state -> { state with scope = declare x v state.scope })
  | Assign (x, e) ->
      let& v =
        match e with
        | AExpr e ->
            let& v = symb_eval_aexpr state.scope e |> lift_fm in
            return (SymbInt v)
        | BExpr e ->
            let& v = symb_eval_bexpr state.scope e |> lift_fm in
            return (SymbBool v)
      in
      modify_state (fun state -> { state with scope = update x v state.scope })
  | Assume e ->
      let& b = lift_fm (symb_eval_bexpr state.scope e) in
      let&* () = Symex.assume [ b ] in
      return ()
  | Assert (e, ln) ->
      let& b = lift_fm (symb_eval_bexpr state.scope e) in
      let&** () =
        Symex.assert_or_error b
          { cause = AssertFail (ln, e); err_stack = state.ok_stack }
      in
      return ()
  | Seq (s1, s2) ->
      let& () = symb_eval_stmt c s1 in
      symb_eval_stmt c s2
  | If (e, then_s, else_s) ->
      let& b = lift_fm (symb_eval_bexpr state.scope e) in
      branch b
        (scoped (symb_eval_stmt c then_s))
        (scoped (symb_eval_stmt c else_s))
  | While (e, s) ->
      let& b = lift_fm (symb_eval_bexpr state.scope e) in
      branch b
        (let& () = scoped (symb_eval_stmt c s) in
         symb_eval_stmt c (While (e, s)))
        (return ())
  | Invoke (svc, args) ->
      let& _ = symb_eval_invoke svc args c.qos in
      return ()
  | DeclareInvoke (x, svc, args) ->
      let& receipt = symb_eval_invoke svc args c.qos in
      modify_state (fun state ->
          { state with scope = declare x receipt state.scope })
  | AssignInvoke (x, serv, args) ->
      let& receipt = symb_eval_invoke serv args c.qos in
      modify_state (fun state ->
          { state with scope = update x receipt state.scope })

(* contract is only needed by build_symb_process, not to evaluate statements *)
let build_symb_process stmt contract policy_init_states =
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
          | TReceipt _ ->
              failwith "Unreachable: receipts cannot be global variables"
        in
        Symex.return (StringMap.add x v acc))
  in
  let scope = [ StringMap.empty; public_env ] in
  let service_map =
    List.fold_left
      (fun m s -> StringMap.add s.name s m)
      StringMap.empty contract.services
  in
  let function_envs =
    List.fold_left
      (fun m f -> StringMap.add f SymbolicListMap.empty m)
      StringMap.empty contract.functions
  in
  ({ scope; function_envs; service_map; ok_stack = [] }, policy_init_states)
  |> run_unit (symb_eval_stmt contract stmt)
  |> Fun.flip Symex.Result.map (fun (s, _) ->
      { s with ok_stack = List.rev s.ok_stack })
  |> Fun.flip Symex.Result.map_error (fun s ->
      { s with err_stack = List.rev s.err_stack })
