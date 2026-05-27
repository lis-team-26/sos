open OrchestratorAST
open Symbolic.Data
open Symbolic.Runtime
open Expr.AST
open Contract.AST
open Utils.Data

let total_env state =
  StringMap.union (fun _ priv _ -> Some priv) state.private_env state.public_env

let build_error msg (ok : ok_monad_state) = { msg; stack = ok.stack }
let init_error msg = Symex.Result.error (build_error msg)
let raise_error msg ok = Symex.Result.error (build_error msg ok)

let seal_error ok partial_err =
  Symex.Result.map_error partial_err (fun partial_err -> partial_err ok)

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
  | EInt n -> Symex.Result.ok (Typed.int n)
  | EBool b -> Symex.Result.ok (Typed.of_bool b)
  | EVar x -> (
      match StringMap.find_opt x env with
      | Some (SymbInt i) -> Symex.Result.ok (Typed.cast i)
      | Some (SymbBool b) -> Symex.Result.ok (Typed.cast b)
      | None -> init_error (Fmt.str "Variable %s not found" x))
  | ENonDet ->
      let* v = Symex.nondet Typed.t_int in
      Symex.Result.ok v
  | EApp (f, args) ->
      init_error "Function application not supported in symbolic execution"
  | EUnOp (op, e) ->
      let** v = symb_eval_expr env e in
      let++ v = symb_eval_bool_un_op op v in
      Typed.cast v
  | EBinOp (e1, op, e2) -> (
      let** v1 = symb_eval_expr env e1 in
      let** v2 = symb_eval_expr env e2 in
      match op with
      | Add | Sub | Mul | Div -> symb_eval_arithm_op v1 op v2
      | And | Or -> symb_eval_bool_bin_op v1 op v2
      | Lt | Le | Gt | Ge | Eq | Neq ->
          let++ v = symb_eval_cmp_op v1 op v2 in
          Typed.cast v)

let symb_eval_aexpr env e =
  let** v = symb_eval_expr env e in
  cast_to_int v

let symb_eval_bexpr env e =
  let** v = symb_eval_expr env e in
  cast_to_bool v

let rec symb_eval_stmt contract state = function
  | Seq (s1, s2) ->
      let** state = symb_eval_stmt contract state s1 in
      symb_eval_stmt contract state s2
  | If (e, then_s, else_s) ->
      let** b = symb_eval_bexpr (total_env state) e |> seal_error state in
      if%sat b then symb_eval_stmt contract state then_s
      else symb_eval_stmt contract state else_s
  | While (e, s) ->
      let** b = symb_eval_bexpr (total_env state) e |> seal_error state in
      if%sat b then
        let** state = symb_eval_stmt contract state s in
        symb_eval_stmt contract state (While (e, s))
      else Symex.Result.ok state
  | stmt -> symb_eval_simple_stmt contract state stmt |> seal_error state

and symb_eval_simple_stmt contract state = function
  | Skip -> Symex.Result.ok state
  | Declare (t, x, e) ->
      let** v = symb_eval_expr (total_env state) e in
      let** env = checked_update_env state.private_env t x v in
      Symex.Result.ok { state with private_env = env }
  | Assign (x, e) ->
      let** v = symb_eval_expr (total_env state) e in
      let** t =
        match StringMap.find_opt x state.private_env with
        | Some (SymbInt _) -> Symex.Result.ok TInt
        | Some (SymbBool _) -> Symex.Result.ok TBool
        | None -> init_error (Fmt.str "Variable %s not declared" x)
      in
      let** env = checked_update_env state.private_env t x v in
      Symex.Result.ok { state with private_env = env }
  | Assume e ->
      let** b = symb_eval_bexpr (total_env state) e in
      let* () = Symex.assume [ b ] in
      Symex.Result.ok state
  | Assert e ->
      let** b = symb_eval_bexpr (total_env state) e in
      let** () =
        Symex.assert_or_error b
          (build_error (Fmt.str "Assertion failed: %a" Typed.ppa b))
      in
      Symex.Result.ok state
  | Invoke (f, args) ->
      let** service =
        match StringMap.find_opt f state.service_map with
        | Some s -> Symex.Result.ok s
        | None -> init_error (Fmt.str "Service %s not found" f)
      in
      let typed_args = List.combine service.params args in
      let** args_env =
        Symex.Result.fold_list typed_args ~init:StringMap.empty
          ~f:(fun acc ((x, t), e) ->
            match t with
            | TInt ->
                let++ arg = symb_eval_aexpr (total_env state) e in
                StringMap.add x (SymbInt arg) acc
            | TBool ->
                let++ arg = symb_eval_bexpr (total_env state) e in
                StringMap.add x (SymbBool arg) acc)
      in
      let precond_env =
        (* The environment for evaluating preconditions gives precedence to the arguments *)
        StringMap.union (fun _ arg _ -> Some arg) args_env state.public_env
      in
      let** b =
        Symex.Result.fold_list service.precond ~init:Typed.v_true
          ~f:(fun acc e ->
            let++ b = symb_eval_bexpr precond_env e in
            acc &&@ b)
      in
      let** () =
        Symex.assert_or_error b
          (build_error (Fmt.str "Precondition violated for service '%s'" f))
      in
      let* success =
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
  | _ as stmt -> symb_eval_simple_stmt contract state stmt

let build_symb_process stmt contract _ =
  let private_env = StringMap.empty in
  let* public_env =
    Symex.fold_list contract.globals ~init:StringMap.empty ~f:(fun acc (x, t) ->
        let* v =
          match t with
          | TInt ->
              (* TODO: initialize with an explicit value or a default value *)
              let* v = Symex.nondet Typed.t_int in
              Symex.return (SymbInt v)
          | TBool ->
              (* TODO: initialize with an explicit value or a default value *)
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
  let state =
    { private_env; public_env; service_map; function_map; stack = [] }
  in
  symb_eval_stmt contract state stmt
  |> Fun.flip Symex.Result.map (fun (s : ok_monad_state) ->
      { s with stack = List.rev s.stack })
  |> Fun.flip Symex.Result.map_error (fun (s : err_monad_state) ->
      { s with stack = List.rev s.stack })
