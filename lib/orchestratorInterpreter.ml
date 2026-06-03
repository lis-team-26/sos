open OrchestratorAST
open Symbolic.Data
open Symbolic.Runtime
open FunctionalStateMonad.FSM
open Expr.AST
open Contract.AST
open Utils.Data

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

let seal_error ok partial_err =
  Symex.Result.map_error partial_err (fun partial_err -> partial_err ok)

let seal_error_ext ext partial_err =
  Symex.Result.map_error partial_err (fun partial_err -> partial_err ext.core)

let cast_to_int v =
  match Typed.cast_checked v Typed.t_int with
  | Some i -> Symex.Result.ok i
  | None -> init_error "Expected an integer"

let cast_to_bool v =
  match Typed.cast_checked v Typed.t_bool with
  | Some b -> Symex.Result.ok b
  | None -> init_error "Expected a boolean"

let checked_update_env env t x v =
  let++ v =
    match t with
    | TInt ->
        let++ v = cast_to_int v in
        SymbInt v
    | TBool ->
        let++ v = cast_to_bool v in
        SymbBool v
  in
  StringMap.add x v env

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
      let& function_map = get_function_map in
      let&+ value =
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
      let&* v = Symex.nondet Typed.t_int in
      return (Typed.cast v)
  | EApp (f, args) -> (
      let& function_map = get_function_map in
      let fun_entry =
        match StringMap.find_opt f function_map with
        | Some entry -> entry
        | None ->
            (* Auto-create uninterpreted function: all args are TInt, returns TInt *)
            let typed_vars = List.map (fun _ -> TInt) args in
            (TFun (typed_vars, TInt), SymbolicListMap.empty)
      in
      let (TFun (typed_vars, ret_type), _) = fun_entry in
      let typed_args = List.combine typed_vars args in
      let& actual_args =
        fold_list typed_args ~init:[] ~f:(fun acc_args (t, x) ->
            match t with
            | TInt ->
                let& v = symb_eval_aexpr env x in
                return (Typed.cast v :: acc_args)
            | TBool ->
                let& v = symb_eval_bexpr env x in
                return (Typed.cast v :: acc_args))
      in
      let actual_args = List.rev actual_args in
      let& function_map = get_function_map in
      let fun_invoke =
        match StringMap.find_opt f function_map with
        | Some (_, fi) -> fi
        | None -> SymbolicListMap.empty
      in
      let&* _, return_value_opt =
        SymbolicListMap.find_opt actual_args fun_invoke
      in
      match return_value_opt with
      | Some (SymbInt i) -> return (Typed.cast i)
      | Some (SymbBool b) -> return (Typed.cast b)
      | None ->
          let&* return_value =
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
            modify_function_map
              (StringMap.add f
                 (TFun (typed_vars, ret_type), new_fun_invoke))
          in
          return return_value)
  | EUnOp (op, e) ->
      let& v = symb_eval_expr env e in
      let&+ v = symb_eval_bool_un_op op v in
      Typed.cast v
  | EBinOp (e1, op, e2) ->
      let& v1 = symb_eval_expr env e1 in
      let& v2 = symb_eval_expr env e2 in
      let&+ result =
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
  let&+ i = cast_to_int v in
  i

and symb_eval_bexpr env e =
  let& v = symb_eval_expr env e in
  let&+ b = cast_to_bool v in
  b

(* --- Service invocation implementation --- *)

(* Apply postcondition effects: given an effect list and an environment,
   evaluate each RHS and update the target variable/function application.
   Uses seal_error to convert partial FSM errors to full err_monad_state errors. *)
let apply_effects effects env function_map state =
  Symex.Result.fold_list effects
    ~init:(env, function_map)
    ~f:(fun (acc_env, acc_fm) (lhs, rhs) ->
      match lhs with
      | LVar x ->
          let++ v, fm =
            symb_eval_expr acc_env rhs acc_fm
            |> seal_error state
          in
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
            Symex.Result.fold_list args
              ~init:([], acc_fm)
              ~f:(fun (acc_args, fm) arg ->
                let++ v, fm =
                  symb_eval_expr acc_env arg fm
                  |> seal_error state
                in
                (v :: acc_args, fm))
          in
          let typed_args = List.rev_map Typed.cast typed_args in
          let++ rhs_val, fm =
            symb_eval_expr acc_env rhs fm
            |> seal_error state
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
                  (ft, SymbolicListMap.syntactic_add typed_args rhs_symb fun_invoke)
                  fm
            | None -> fm
          in
          (acc_env, fm))

(* Assume all constraints hold *)
let assume_constraints constraints env function_map state =
  Symex.Result.fold_list constraints
    ~init:(Typed.v_true, function_map)
    ~f:(fun (acc_conj, acc_fm) expr ->
      let++ b, fm =
        symb_eval_bexpr env expr acc_fm
        |> seal_error state
      in
      (acc_conj &&@ b, fm))

(* Map string errors from policy checker to err_monad_state *)
let map_policy_error state result =
  Symex.Result.map_error result (fun msg -> build_error msg state)

(* Main service invocation logic.
   Returns: (return_value option * updated_extended_state) in the symbolic monad *)
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
          symb_eval_bexpr precond_env e acc_function_map
          |> seal_error state
        in
        (acc_precond &&@ b, function_map))
  in
  let** () =
    Symex.assert_or_error precond_conj
      (build_error
         (Fmt.str "Precondition violated for service '%s'" service.name)
         state)
  in
  let state = { state with function_map } in

  (* 3. Create fresh QoS values *)
  let qos_fields_from_effects =
    List.filter_map
      (fun (lhs, _) ->
        match lhs with LVar x -> Some x | LApp _ -> None)
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
    StringMap.union (fun _ _ arg -> Some arg) state.public_env
      (StringMap.union (fun _ _ arg -> Some arg) args_env qos_env)
  in
  let** qos_env', function_map =
    apply_effects (fst service.qos_postcond) qos_eval_env state.function_map state
  in
  (* Filter only QoS fields from the result *)
  let qos_effect_names =
    List.filter_map
      (fun (lhs, _) ->
        match lhs with LVar x -> Some x | LApp _ -> None)
      (fst service.qos_postcond)
  in
  let qos_env =
    StringMap.filter
      (fun k _ ->
        List.mem k qos_effect_names
        || StringMap.mem k qos_env)
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
      StringMap.union (fun _ _ arg -> Some arg) state.public_env
        (StringMap.union (fun _ _ arg -> Some arg) args_env qos_env)
    in
    let** ok_env', function_map =
      apply_effects (fst service.ok_postcond) ok_eval_env state.function_map state
    in
    let** ok_conj, function_map =
      assume_constraints (snd service.ok_postcond) ok_env' function_map state
    in
    let* () = Symex.assume [ ok_conj ] in

    let public_env =
      StringMap.fold
        (fun k v acc ->
          if StringMap.mem k state.public_env then StringMap.add k v acc else acc)
        ok_env' state.public_env
    in

    let ret_var_name, _ret_var_type = service.returns in
    let ret_value = StringMap.find_opt ret_var_name ok_env' in

    let invocation =
      {
        service;
        actual_args = args_env;
        failed = Typed.v_false;
        qos = qos_env;
      }
    in
    let** policy_checkers =
      Symex.Result.map_list ext.policy_checkers
        ~f:(fun checker ->
          let++ updated =
            PolicyChecker.update_policy invocation checker
            |> map_policy_error state
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
  else begin
    (* FAILURE path *)
    match service.err_postcond with
    | None -> assert false
    | Some err_postcond ->
        let err_eval_env =
          StringMap.union (fun _ _ arg -> Some arg) state.public_env
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
          Symex.Result.map_list ext.policy_checkers
            ~f:(fun checker ->
              let++ updated =
                PolicyChecker.update_policy invocation checker
                |> map_policy_error state
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

(* --- Statement evaluation --- *)

let rec symb_eval_stmt ext = function
  | Seq (s1, s2) ->
      let** ext = symb_eval_stmt ext s1 in
      symb_eval_stmt ext s2
  | If (e, then_s, else_s) ->
      let** b, function_map =
        symb_eval_bexpr (total_env ext.core) e ext.core.function_map
        |> seal_error ext.core
      in
      let ext = { ext with core = { ext.core with function_map } } in
      if%sat b then symb_eval_stmt ext then_s else symb_eval_stmt ext else_s
  | While (e, s) ->
      let** b, function_map =
        symb_eval_bexpr (total_env ext.core) e ext.core.function_map
        |> seal_error ext.core
      in
      let ext = { ext with core = { ext.core with function_map } } in
      if%sat b then
        let** ext = symb_eval_stmt ext s in
        symb_eval_stmt ext (While (e, s))
      else Symex.Result.ok ext
  | Invoke (f, args) ->
      let state = ext.core in
      let** service =
        match StringMap.find_opt f state.service_map with
        | Some s -> Symex.Result.ok s
        | None -> Symex.Result.error (build_error (Fmt.str "Service %s not found" f) state)
      in
      let** _ret_value, ext = symb_invoke ext service args in
      Symex.Result.ok ext
  | DeclareInvoke (t, x, f, args) ->
      let state = ext.core in
      let** service =
        match StringMap.find_opt f state.service_map with
        | Some s -> Symex.Result.ok s
        | None -> Symex.Result.error (build_error (Fmt.str "Service %s not found" f) state)
      in
      let** ret_value_opt, ext = symb_invoke ext service args in
      let state = ext.core in
      (* Get the return value or create a fresh nondeterministic one *)
      let* ret_val =
        match ret_value_opt with
        | Some v -> Symex.return v
        | None -> (
            match t with
            | TInt ->
                let* v = Symex.nondet Typed.t_int in
                Symex.return (SymbInt v)
            | TBool ->
                let* v = Symex.nondet Typed.t_bool in
                Symex.return (SymbBool v))
      in
      let v =
        match ret_val with
        | SymbInt i -> Typed.cast i
        | SymbBool b -> Typed.cast b
      in
      let++ private_env =
        checked_update_env state.private_env t x v
        |> seal_error state
      in
      { ext with core = { state with private_env } }
  | AssignInvoke (x, f, args) ->
      let state = ext.core in
      let** service =
        match StringMap.find_opt f state.service_map with
        | Some s -> Symex.Result.ok s
        | None -> Symex.Result.error (build_error (Fmt.str "Service %s not found" f) state)
      in
      let** t =
        match StringMap.find_opt x state.private_env with
        | Some (SymbInt _) -> Symex.Result.ok TInt
        | Some (SymbBool _) -> Symex.Result.ok TBool
        | None -> Symex.Result.error (build_error (Fmt.str "Variable %s not declared" x) state)
      in
      let** ret_value_opt, ext = symb_invoke ext service args in
      let state = ext.core in
      let* ret_val =
        match ret_value_opt with
        | Some v -> Symex.return v
        | None -> (
            match t with
            | TInt ->
                let* v = Symex.nondet Typed.t_int in
                Symex.return (SymbInt v)
            | TBool ->
                let* v = Symex.nondet Typed.t_bool in
                Symex.return (SymbBool v))
      in
      let v =
        match ret_val with
        | SymbInt i -> Typed.cast i
        | SymbBool b -> Typed.cast b
      in
      let++ private_env =
        checked_update_env state.private_env t x v
        |> seal_error state
      in
      { ext with core = { state with private_env } }
  | stmt -> symb_eval_simple_stmt ext stmt |> seal_error_ext ext

and symb_eval_simple_stmt ext = function
  | Skip -> Symex.Result.ok ext
  | Declare (t, x, e) -> (
      let state = ext.core in
      match t with
      | TInt ->
          let** v, function_map =
            symb_eval_aexpr (total_env state) e state.function_map
          in
          let++ private_env = checked_update_env state.private_env t x v in
          { ext with core = { state with private_env; function_map } }
      | TBool ->
          let** v, function_map =
            symb_eval_bexpr (total_env state) e state.function_map
          in
          let++ private_env = checked_update_env state.private_env t x v in
          { ext with core = { state with private_env; function_map } })
  | Assign (x, e) ->
      let state = ext.core in
      let** t =
        match StringMap.find_opt x state.private_env with
        | Some (SymbInt _) -> Symex.Result.ok TInt
        | Some (SymbBool _) -> Symex.Result.ok TBool
        | None -> init_error (Fmt.str "Variable %s not declared" x)
      in
      let** v, function_map =
        match t with
        | TInt -> symb_eval_aexpr (total_env state) e state.function_map
        | TBool -> symb_eval_bexpr (total_env state) e state.function_map
      in
      let++ private_env = checked_update_env state.private_env t x v in
      { ext with core = { state with private_env; function_map } }
  | Assume e ->
      let state = ext.core in
      let** b, function_map =
        symb_eval_bexpr (total_env state) e state.function_map
      in
      let* () = Symex.assume [ b ] in
      Symex.Result.ok { ext with core = { state with function_map } }
  | Assert e ->
      let state = ext.core in
      let** b, function_map =
        symb_eval_bexpr (total_env state) e state.function_map
      in
      let++ () =
        Symex.assert_or_error b
          (fun state -> build_error (Fmt.str "Assertion failed: %a" Typed.ppa b) state)
      in
      { ext with core = { state with function_map } }
  | _ as stmt -> symb_eval_simple_stmt ext stmt

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
  let ext = { core; policy_checkers = policy_init_states } in
  symb_eval_stmt ext stmt
  |> Fun.flip Symex.Result.map (fun (ext : extended_state) ->
      { ext.core with stack = List.rev ext.core.stack })
  |> Fun.flip Symex.Result.map_error (fun (s : err_monad_state) ->
      { s with stack = List.rev s.stack })
