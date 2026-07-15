open TypedOrchestratorAST
open Expr.TypedAST
open Contract.TypedAST
open ExprInterpreter
open Symbolic.Data
open Symbolic.Runtime
open StateMonad.StatementMonad
open StateMonad.Utils
open PolicyChecker.Logic
open Utils.Data
open Utils.Loc
open Utils.Scope
open Utils.Types

let symb_eval_constraints ~scope constraints =
  fold_list constraints ~init:Typed.v_true ~f:(fun acc e ->
      let&&+ b = symb_eval_bexpr ~scope e in
      acc &&@ b)

let symb_eval_effects ~scope effects =
  fold_list effects ~init:scope ~f:(fun scope (lhs, rhs) ->
      match lhs with
      | LVar x ->
          let&&+ v = symb_eval_expr ~scope rhs in
          update x v scope
      | LApp (f, args) ->
          let& actual_args =
            map_list args ~f:(fun e ->
                let&&+ v = symb_eval_expr ~scope e in
                cast v)
          in
          let&&* v = symb_eval_expr ~scope rhs in
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

let symb_eval_postcond ~scope postcond =
  let effects, constraints = postcond in
  let& scope = symb_eval_effects ~scope effects in
  let& constraints = symb_eval_constraints ~scope constraints in
  let&* () = Symex.assume [ constraints ] in
  return scope

let symb_eval_invoke ~qos_fields ~loc svc_name args =
  let& state = get_state in
  let pre_invoke_scope = state.scope in
  let svc =
    match StringMap.find_opt svc_name state.service_map with
    | Some svc -> svc
    | None -> failwith "Unreachable: service not found in environment"
  in
  let actual_args = List.combine svc.params args in
  let& actual_args_env =
    fold_list actual_args ~init:StringMap.empty ~f:(fun env (x, e) ->
        match e with
        | AExpr e ->
            let&&+ v = symb_eval_aexpr ~scope:pre_invoke_scope e in
            let v = SymbInt v in
            StringMap.add x v env
        | BExpr e ->
            let&&+ v = symb_eval_bexpr ~scope:pre_invoke_scope e in
            let v = SymbBool v in
            StringMap.add x v env)
  in
  let precond_scope = [ actual_args_env; get_public_env pre_invoke_scope ] in
  let& b =
    fold_list svc.precond ~init:Typed.v_true ~f:(fun acc_precond e ->
        let&&+ b = symb_eval_bexpr ~scope:precond_scope e in
        acc_precond &&@ b)
  in
  let&** () =
    located ~loc (PrecondError svc)
    |> error_from_cause ~last_ok:state
    |> Symex.assert_or_error b
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
    symb_eval_postcond ~scope:(qos_env :: precond_scope) svc.qos_postcond
  in
  let qos_env = List.hd qos_scope in
  let&* ret_var, nondet_ret_val =
    match svc.returns with
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
    match svc.err_postcond with
    | None -> Symex.return Typed.v_true
    | Some _ -> Symex.nondet Typed.t_bool
  in
  let& postcond_scope =
    branch successful
      (fun () -> symb_eval_postcond ~scope:postcond_scope svc.ok_postcond)
      (fun () ->
        let err_postcond =
          match svc.err_postcond with
          | Some err_postcond -> err_postcond
          | None ->
              failwith
                "Unreachable: if service has no error postcondition, it must \
                 be successful"
        in
        symb_eval_postcond ~scope:postcond_scope err_postcond)
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
      service = svc;
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
          history = invocation :: state.history;
        })
  in
  let& state, policy_checkers = get in
  let& policy_checkers =
    map_list policy_checkers ~f:(fun checker ->
        let&** checker =
          update_policy ~loc invocation checker |> map_error ~last_ok:state
        in
        return checker)
  in
  let& () = modify_policy_checkers (fun _ -> policy_checkers) in
  return (SymbReceipt { ret_val; successful; qos_fields = qos_env })

let rec symb_eval_stmt ~contract stmt =
  let loc = stmt.at in
  let& state = get_state in
  let& () =
    match stmt.it with Seq _ -> return () | _ -> consume_steps_fuel 1
  in
  match stmt.it with
  | Skip -> return ()
  | Declare (x, e) ->
      let& v =
        match e with
        | AExpr e ->
            let&&+ v = symb_eval_aexpr ~scope:state.scope e in
            SymbInt v
        | BExpr e ->
            let&&+ v = symb_eval_bexpr ~scope:state.scope e in
            SymbBool v
      in
      modify_state (fun state -> { state with scope = declare x v state.scope })
  | Assign (x, e) ->
      let& v =
        match e with
        | AExpr e ->
            let&&+ v = symb_eval_aexpr ~scope:state.scope e in
            SymbInt v
        | BExpr e ->
            let&&+ v = symb_eval_bexpr ~scope:state.scope e in
            SymbBool v
      in
      modify_state (fun state -> { state with scope = update x v state.scope })
  | Assume e ->
      let&&* b = symb_eval_bexpr ~scope:state.scope e in
      let&* () = Symex.assume [ b ] in
      return ()
  | Assert e ->
      let&&* b = symb_eval_bexpr ~scope:state.scope e in
      let&** () =
        AssertionError e |> located ~loc
        |> error_from_cause ~last_ok:state
        |> Symex.assert_or_error b
      in
      return ()
  | Seq (s1, s2) ->
      let& () = symb_eval_stmt ~contract s1 in
      symb_eval_stmt ~contract s2
  | If (e, then_s, else_s) ->
      let&&* b = symb_eval_bexpr ~scope:state.scope e in
      branch b
        (fun () -> scoped (symb_eval_stmt ~contract then_s))
        (fun () -> scoped (symb_eval_stmt ~contract else_s))
  | While (e, s) ->
      let&&* b = symb_eval_bexpr ~scope:state.scope e in
      branch b
        (fun () ->
          let& () = scoped (symb_eval_stmt ~contract s) in
          let& () = consume_unroll_fuel 1 in
          symb_eval_stmt ~contract stmt)
        (fun () -> return ())
  | Invoke (svc, args) ->
      let& _ = symb_eval_invoke ~qos_fields:contract.qos ~loc svc args in
      return ()
  | DeclareInvoke (x, svc_name, args) ->
      let& receipt =
        symb_eval_invoke ~qos_fields:contract.qos ~loc svc_name args
      in
      modify_state (fun state ->
          { state with scope = declare x receipt state.scope })
  | AssignInvoke (x, svc_name, args) ->
      let& receipt =
        symb_eval_invoke ~qos_fields:contract.qos ~loc svc_name args
      in
      modify_state (fun state ->
          { state with scope = update x receipt state.scope })

let build_symex_process ~fuel contract orchestrator =
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
  let svc_map =
    List.fold_left
      (fun m s -> StringMap.add s.name s m)
      StringMap.empty contract.services
  in
  let fun_envs =
    List.fold_left
      (fun m f -> StringMap.add f SymbolicListMap.empty m)
      StringMap.empty contract.functions
  in
  let state =
    {
      scope;
      history = [];
      fuel;
      function_envs = fun_envs;
      service_map = svc_map;
    }
  in
  let policy_checkers =
    List.mapi (fun id -> build_policy_checker (id + 1)) contract.policies
  in
  let process =
    (* Evaluate and assume the assumptions on global variables *)
    let& b =
      map_list contract.globals_assumptions ~f:(fun e ->
          let&&* b = symb_eval_bexpr ~scope e in
          return b)
    in
    let&* () = Symex.assume b in
    (* Run the orchestrator code *)
    let& () = symb_eval_stmt ~contract orchestrator in
    (* Update the checkers enforcing deferred policies *)
    let& state, policy_checkers = get in
    let& () =
      fold_list policy_checkers ~init:() ~f:(fun () checker ->
          let&** () = verify_policy checker |> map_error ~last_ok:state in
          return ())
    in
    return ()
  in
  (state, policy_checkers) |> run_unit process
  |> Fun.flip Symex.Result.map (fun (s, _) ->
      { s with history = List.rev s.history })
  |> Fun.flip Symex.Result.map_error (function
    | Err s -> Err { s with err_history = List.rev s.err_history }
    | Unexplored s -> Unexplored { s with history = List.rev s.history })
