open TypedOrchestratorAST
open Expr.TypedAST
open Contract.TypedAST
open Symbolic.Data
open Symbolic.Runtime
open StateMonad
open StateMonad.FunctionalMonad
open StateMonad.OkStateMonad
open Utils.Data
open Utils.Types

(* Extended state that carries policy checkers alongside the core monad state *)
type extended_state = {
  core : ok_state;
  policy_checkers : PolicyChecker.pChecker list;
}

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
  | BAccess (x, field) ->
      failwith "Not yet implemented: symbolic boolean field access"
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

let symb_eval_constraints env constraints =
  fold_list constraints ~init:Typed.v_true ~f:(fun acc e ->
      let& b = symb_eval_bexpr env e in
      return (acc &&@ b))

let symb_apply_effects scope effects =
  fold_list effects ~init:scope ~f:(fun scope (lhs, rhs) ->
      match lhs with
      | LVar x ->
          let& v = lift_fm (symb_eval_expr scope rhs) in
          return (update x v scope)
      | LApp (f, args) ->
          let& actual_args =
            lift_fm
              (map_list args ~f:(fun e ->
                   let& v = symb_eval_expr scope e in
                   return (cast v)))
          in
          let& v = lift_fm (symb_eval_expr scope rhs) in
          let& state = get in
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
          let& () = put { state with function_envs = fun_envs } in
          return scope)

let symb_eval_postcond postcond scope service actual_args success qos =
  let effects, constraints = postcond in
  let& scope = symb_apply_effects scope effects in
  let& constraints = lift_fm (symb_eval_constraints scope constraints) in
  let&* () = Symex.assume [ constraints ] in
  modify (fun state ->
      {
        state with
        env = pop_scope scope;
        ok_stack =
          { service; actual_args; successful = success; qos } :: state.ok_stack;
      })

let rec symb_eval_stmt c stmt =
  let& state = get in
  match stmt with
  | Skip -> return ()
  | Declare (x, e) ->
      let& v =
        lift_fm
          (match e with
          | AExpr e ->
              let& v = symb_eval_aexpr state.env e in
              return (SymbInt v)
          | BExpr e ->
              let& v = symb_eval_bexpr state.env e in
              return (SymbBool v))
      in
      put { state with env = declare x v state.env }
  | Assign (x, e) ->
      let& v =
        lift_fm
          (match e with
          | AExpr e ->
              let& v = symb_eval_aexpr state.env e in
              return (SymbInt v)
          | BExpr e ->
              let& v = symb_eval_bexpr state.env e in
              return (SymbBool v))
      in
      put { state with env = update x v state.env }
  | Assume e ->
      let& b = lift_fm (symb_eval_bexpr state.env e) in
      let&* () = Symex.assume [ b ] in
      return ()
  | Assert e ->
      let& b = lift_fm (symb_eval_bexpr state.env e) in
      let&** () =
        Symex.assert_or_error b
          {
            msg = Fmt.str "Assertion failed: %a" Typed.ppa b;
            err_stack = state.ok_stack;
          }
      in
      return ()
  | Seq (s1, s2) ->
      let& () = symb_eval_stmt c s1 in
      symb_eval_stmt c s2
  | If (e, then_s, else_s) ->
      let& b = lift_fm (symb_eval_bexpr state.env e) in
      branch b
        (scoped (symb_eval_stmt c then_s))
        (scoped (symb_eval_stmt c else_s))
  | While (e, s) ->
      let& b = lift_fm (symb_eval_bexpr state.env e) in
      branch b
        (let& () = scoped (symb_eval_stmt c s) in
         symb_eval_stmt c (While (e, s)))
        (return ())
  | Invoke (svc, args) ->
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
                let& v = lift_fm (symb_eval_aexpr state.env e) in
                let v = SymbInt v in
                return (StringMap.add x v env)
            | BExpr e ->
                let& v = lift_fm (symb_eval_bexpr state.env e) in
                let v = SymbBool v in
                return (StringMap.add x v env))
      in
      let precond_scope = [ actual_args_env; get_public_env state.env ] in
      let& () = put { state with env = precond_scope } in
      let& b =
        lift_fm
          (fold_list service.precond ~init:Typed.v_true ~f:(fun acc_precond e ->
               let& b = symb_eval_bexpr precond_scope e in
               return (acc_precond &&@ b)))
      in
      let&** () =
        Symex.assert_or_error b
          {
            msg = Fmt.str "Precondition violated for service '%s'" svc;
            err_stack = state.ok_stack;
          }
      in
      let& qos_env =
        fold_list
          Contract.TypedAST.(c.qos)
          ~init:StringMap.empty
          ~f:(fun env (x, t) ->
            match t with
            | TInt ->
                let&* v = Symex.nondet Typed.t_int in
                let v = SymbInt v in
                return (StringMap.add x v env)
            | TBool ->
                let&* v = Symex.nondet Typed.t_bool in
                let v = SymbBool v in
                return (StringMap.add x v env)
            | TReceipt _ ->
                failwith "Unreachable: receipts cannot be QoS fields")
      in
      let qos_scope = qos_env :: precond_scope in
      let& qos_scope =
        symb_apply_effects qos_scope (fst service.qos_postcond)
      in
      let& qos_constraints =
        lift_fm (symb_eval_constraints qos_scope (snd service.qos_postcond))
      in
      let&* () = Symex.assume [ qos_constraints ] in
      let&* success =
        if Option.is_none service.err_postcond then Symex.return Typed.v_true
        else Symex.nondet Typed.t_bool
      in
      let qos = List.hd qos_scope in
      let&** state =
        if%sat success then
          let* x, retval =
            match service.returns with
            | x, TInt ->
                let+ v = Symex.nondet Typed.t_int in
                (x, SymbInt v)
            | x, TBool ->
                let+ v = Symex.nondet Typed.t_bool in
                (x, SymbBool v)
            | _ ->
                failwith "Unreachable: service return type must be int or bool"
          in
          let ok_postcond_scope =
            qos_scope |> pop_scope |> push_scope |> declare x retval
          in
          run_unit
            (symb_eval_postcond service.ok_postcond ok_postcond_scope service
               actual_args_env success qos)
            state
        else
          match service.err_postcond with
          | Some err_postcond ->
              run_unit
                (symb_eval_postcond err_postcond qos_scope service
                   actual_args_env success qos)
                state
          | None -> run_unit (return ()) state
      in
      put state
  | DeclareInvoke (x, svc, args) -> failwith "Not yet implemented"
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
          | TReceipt _ ->
              failwith "Unreachable: receipts cannot be global variables"
        in
        Symex.return (StringMap.add x v acc))
  in
  let scope = [ private_env; public_env ] in
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
  let state = { env = scope; function_envs; service_map; ok_stack = [] } in
  let symbolic_results = run_unit (symb_eval_stmt contract stmt) state in
  symbolic_results
  |> Fun.flip Symex.Result.map (fun (s : ok_state) ->
      { s with ok_stack = List.rev s.ok_stack })
  |> Fun.flip Symex.Result.map_error (fun (s : err_state) ->
      { s with err_stack = List.rev s.err_stack })

(* let apply_effects effects env function_map state =
  Symex.Result.fold_list effects ~init:(env, function_map)
    ~f:(fun (acc_env, acc_fm) (lhs, rhs) ->
      match lhs with
      | LVar x ->
          let++ v, fm = symb_eval_expr acc_env rhs acc_fm |> seal_error state in
          let acc_env =
            match Typed.cast_checked v Typed.t_int with
            | Some i -> StringMap.add x (SymbInt i) acc_env
            | None -> (
                match Typed.cast_checked v Typed.t_bool with
                | Some b -> StringMap.add x (SymbBool b) acc_env
                | None -> acc_env)
          in
          (acc_env, fm)
      | LApp (f, args) ->
          (* Function application effect: f(args) := rhs *)
          let** typed_args, fm =
            Symex.Result.fold_list args ~init:([], acc_fm)
              ~f:(fun (acc_args, fm) arg ->
                let++ v, fm =
                  symb_eval_expr acc_env arg fm |> seal_error state
                in
                (v :: acc_args, fm))
          in
          let typed_args = List.rev_map Typed.cast typed_args in
          let++ rhs_val, fm =
            symb_eval_expr acc_env rhs fm |> seal_error state
          in
          let rhs_symb =
            match Typed.cast_checked rhs_val Typed.t_int with
            | Some i -> SymbInt i
            | None -> (
                match Typed.cast_checked rhs_val Typed.t_bool with
                | Some b -> SymbBool b
                | None -> SymbInt (Typed.cast rhs_val))
          in
          let fm =
            match StringMap.find_opt f fm with
            | Some (ft, fun_invoke) ->
                StringMap.add f
                  ( ft,
                    SymbolicListMap.syntactic_add typed_args rhs_symb fun_invoke
                  )
                  fm
            | None -> fm
          in
          (acc_env, fm))

let assume_constraints constraints env function_map state =
  Symex.Result.fold_list constraints ~init:(Typed.v_true, function_map)
    ~f:(fun (acc_conj, acc_fm) expr ->
      let++ b, fm = symb_eval_bexpr env expr acc_fm |> seal_error state in
      (acc_conj &&@ b, fm))

let symb_invoke ext service args_exprs =
  let state = ext.core in
  (* 1. Evaluate arguments *)
  let typed_params_args = List.combine service.params args_exprs in
  let** args_env, function_map =
    Symex.Result.fold_list typed_params_args
      ~init:(StringMap.empty, state.function_map)
      ~f:(fun (acc_args_env, acc_function_map) ((x, t), e) ->
        match t with
        | TInt ->
            let++ arg, function_map =
              symb_eval_aexpr (total_env state) e acc_function_map
              |> seal_error state
            in
            (StringMap.add x (SymbInt arg) acc_args_env, function_map)
        | TBool ->
            let++ arg, function_map =
              symb_eval_bexpr (total_env state) e acc_function_map
              |> seal_error state
            in
            (StringMap.add x (SymbBool arg) acc_args_env, function_map))
  in
  let state = { state with function_map } in

  (* 2. Check preconditions *)
  let precond_env =
    StringMap.union (fun _ arg _ -> Some arg) args_env state.public_env
  in
  let** precond_conj, function_map =
    Symex.Result.fold_list service.precond
      ~init:(Typed.v_true, state.function_map)
      ~f:(fun (acc_precond, acc_function_map) e ->
        let++ b, function_map =
          symb_eval_bexpr precond_env e acc_function_map |> seal_error state
        in
        (acc_precond &&@ b, function_map))
  in
  let** () =
    Symex.assert_or_error precond_conj
      {
        msg = Fmt.str "Precondition violated for service '%s'" service.name;
        stack = state.stack;
      }
  in
  let state = { state with function_map } in

  (* 3. Create fresh QoS values *)
  let qos_fields_from_effects =
    List.filter_map
      (fun (lhs, _) -> match lhs with LVar x -> Some x | LApp _ -> None)
      (fst service.qos_postcond)
  in
  let qos_fields_from_constraints =
    List.concat_map
      (fun expr -> StringSet.elements (Expr.free_vars expr))
      (snd service.qos_postcond)
  in
  let qos_fields =
    List.sort_uniq String.compare
      (qos_fields_from_effects @ qos_fields_from_constraints)
  in
  let* qos_env =
    Symex.fold_list qos_fields ~init:StringMap.empty ~f:(fun acc field ->
        let* v = Symex.nondet Typed.t_int in
        Symex.return (StringMap.add field (SymbInt v) acc))
  in

  (* 4. Apply QoS postcondition effects *)
  let qos_eval_env =
    StringMap.union
      (fun _ _ arg -> Some arg)
      state.public_env
      (StringMap.union (fun _ _ arg -> Some arg) args_env qos_env)
  in
  let** qos_env', function_map =
    apply_effects (fst service.qos_postcond) qos_eval_env state.function_map
      state
  in
  (* Filter only QoS fields from the result *)
  let qos_effect_names =
    List.filter_map
      (fun (lhs, _) -> match lhs with LVar x -> Some x | LApp _ -> None)
      (fst service.qos_postcond)
  in
  let qos_env =
    StringMap.filter
      (fun k _ -> List.mem k qos_effect_names || StringMap.mem k qos_env)
      qos_env'
  in
  (* Assume QoS constraints *)
  let** qos_conj, function_map =
    assume_constraints (snd service.qos_postcond) qos_env' function_map state
  in
  let* () = Symex.assume [ qos_conj ] in
  let state = { state with function_map } in

  (* 5. Branch on success/failure *)
  let* success =
    if Option.is_none service.err_postcond then Symex.return Typed.v_true
    else Symex.nondet Typed.t_bool
  in

  if%sat success then begin
    (* SUCCESS path *)
    let ok_eval_env =
      StringMap.union
        (fun _ _ arg -> Some arg)
        state.public_env
        (StringMap.union (fun _ _ arg -> Some arg) args_env qos_env)
    in
    let** ok_env', function_map =
      apply_effects (fst service.ok_postcond) ok_eval_env state.function_map
        state
    in
    let** ok_conj, function_map =
      assume_constraints (snd service.ok_postcond) ok_env' function_map state
    in
    let* () = Symex.assume [ ok_conj ] in

    let public_env =
      StringMap.fold
        (fun k v acc ->
          if StringMap.mem k state.public_env then StringMap.add k v acc
          else acc)
        ok_env' state.public_env
    in

    let ret_var_name, _ret_var_type = service.returns in
    let ret_value = StringMap.find_opt ret_var_name ok_env' in

    let invocation =
      { service; actual_args = args_env; failed = Typed.v_false; qos = qos_env }
    in
    let** policy_checkers =
      Symex.Result.map_list ext.policy_checkers ~f:(fun checker ->
          let++ updated =
            PolicyChecker.update_policy invocation checker |> seal_error state
          in
          updated)
    in
    let state =
      {
        state with
        public_env;
        function_map;
        stack = state.stack @ [ invocation ];
      }
    in
    Symex.Result.ok (ret_value, { core = state; policy_checkers })
  end
  else
    (* FAILURE path *)
    begin match service.err_postcond with
    | None -> assert false
    | Some err_postcond ->
        let err_eval_env =
          StringMap.union
            (fun _ _ arg -> Some arg)
            state.public_env
            (StringMap.union (fun _ _ arg -> Some arg) args_env qos_env)
        in
        let** err_env', function_map =
          apply_effects (fst err_postcond) err_eval_env state.function_map state
        in
        let** err_conj, function_map =
          assume_constraints (snd err_postcond) err_env' function_map state
        in
        let* () = Symex.assume [ err_conj ] in

        let public_env =
          StringMap.fold
            (fun k v acc ->
              if StringMap.mem k state.public_env then StringMap.add k v acc
              else acc)
            err_env' state.public_env
        in

        let ret_var_name, _ret_var_type = service.returns in
        let ret_value = StringMap.find_opt ret_var_name err_env' in

        let invocation =
          {
            service;
            actual_args = args_env;
            failed = Typed.v_false;
            qos = qos_env;
          }
        in
        let** policy_checkers =
          Symex.Result.map_list ext.policy_checkers ~f:(fun checker ->
              let++ updated =
                PolicyChecker.update_policy invocation checker
                |> seal_error state
              in
              updated)
        in

        let state =
          {
            state with
            public_env;
            function_map;
            stack = state.stack @ [ invocation ];
          }
        in
        Symex.Result.ok (ret_value, { core = state; policy_checkers })
    end *)
