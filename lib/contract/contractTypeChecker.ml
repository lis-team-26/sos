open ContractAST
module TC = TypedContractAST
open Expr.AST
module TE = Expr.TypedAST
open Expr.TypeChecker
open Utils.Data
open Utils.Loc
open Utils.Result
open Result.Syntax

(** Builds a set of names from a list of located names, ensuring that no
    duplicate name is present. Returns [Ok set] if the check passes, or an error
    message with the location of the duplicate name if it fails. *)
let build_set ~pp_err names =
  List.fold_left
    (fun acc { it = name; at = loc } ->
      let* acc = acc in
      if StringSet.mem name acc then located_error ~loc "%a" pp_err name
      else Ok (StringSet.add name acc))
    (Ok StringSet.empty) names

(** Builds a variable-type environment from a list of typed variables, also
    ensuring that no duplicate name is present in. Returns [Ok map] if the check
    passes, or an error message with the location of the duplicate name if it
    fails. *)
let build_env ~pp_err typed_names =
  List.fold_left
    (fun acc { it = x, t; at = loc } ->
      let* acc = acc in
      match StringMap.find_opt x acc with
      | None -> Ok (StringMap.add x t acc)
      | Some _ -> located_error ~loc "%a" pp_err x)
    (Ok StringMap.empty) typed_names

(** Casts a binary operator to a comparison operator, if possible. *)
let cmp_op_of_bin_op ~loc = function
  | Eq -> Ok TE.Eq
  | Neq -> Ok TE.Neq
  | Lt -> Ok TE.Lt
  | Le -> Ok TE.Le
  | Gt -> Ok TE.Gt
  | Ge -> Ok TE.Ge
  | Add | Sub | Mul | Div | And | Or ->
      located_error ~loc "QoS policy must use a comparison operator"

(** Ensures that an expression does not contain function applications. If it
    does, returns an error with the source code location of the function
    application. *)
let without_fun_app e =
  match has_fun_app e with
  | None -> Ok e
  | Some loc -> located_error ~loc "Function applications are not allowed here"

(** Type-checks a policy *)
let type_check_policy ~services_names ~qos_fields policy =
  let (policy_type, group_by), loc = (policy.it, policy.at) in
  match policy_type with
  | QosFieldOp (aggr, field, op, threshold) ->
      let* () =
        (* The aggregated field must be defined in the QoS fields *)
        if StringSet.mem field qos_fields then Ok ()
        else located_error ~loc "Undefined '%s' QoS field" field
      in
      (* A comparison operator must be used *)
      let* cmp = cmp_op_of_bin_op ~loc op in
      Ok (TC.QosFieldOp (aggr, field, cmp, threshold), group_by)
  | Regex (s2l, regex) ->
      (* Bad prefix regex must use only defined service names *)
      let undefined_services =
        List.filter (fun (s, _) -> not (StringSet.mem s services_names)) s2l
      in
      let* () =
        match undefined_services with
        | [] -> Ok ()
        | (svc, _) :: _ ->
            located_error ~loc "Unknown service '%s' in regex policy" svc
      in
      (* Checks if the regex is well-formed, to avoid runtime errors in the regex-to-DFA conversion *)
      let* () =
        try
          let domain = CharSet.of_list @@ List.map snd s2l in
          let _ = Reg2dfa.Regex.reg2dfa ~domain regex in
          Ok ()
        with Reg2dfa.Regex.Parse_error _ ->
          located_error ~loc "Malformed regex"
      in
      Ok (TC.Regex (s2l, regex), group_by)
  | Sort field ->
      (* The sorted field must be defined in the QoS fields *)
      if StringSet.mem field qos_fields then Ok (TC.Sort field, group_by)
      else located_error ~loc "Undefined '%s' QoS field" field

(** Type-checks a single effect [lhs := rhs].
    - [lhs_type_env] resolves the declared type of an assigned variable
      ([LVar]);
    - [args_scope] is the scope used to type-check arguments in an function
      application lhs ([LApp]);
    - [rhs_scope] is the scope used to type-check the right-hand side
      expression;
    - [fun_env] is the environment containing function signatures. *)
let type_check_effect ~lhs_type_env ~args_scope ~rhs_scope ~fun_env (lhs, rhs) =
  match lhs.it with
  | LVar v -> (
      match StringMap.find_opt v lhs_type_env with
      | None ->
          located_error ~loc:lhs.at "Effect assigns to unknown variable %s" v
      | Some t ->
          let* typed_rhs = type_check_expr ~scope:rhs_scope ~fun_env t rhs in
          Ok (TC.LVar v, typed_rhs))
  | LApp (f, args) -> (
      match StringMap.find_opt f fun_env with
      | None ->
          located_error ~loc:lhs.at "Effect assigns to unknown function %s" f
      | Some (TFun (_, ret_type)) ->
          let* typed_args =
            type_check_app ~scope:args_scope ~fun_env ~loc:lhs.at f args
          in
          let* typed_rhs =
            type_check_expr ~scope:rhs_scope ~fun_env ret_type rhs
          in
          Ok (TC.LApp (f, typed_args), typed_rhs))

(** Type-checks a postcondition; see [type_check_effect] for details.
    [constr_scope] is the scope used to type-check the boolean constraints. *)
let type_check_postcond ~lhs_type_env ~args_scope ~rhs_scope ~constr_scope
    ~fun_env (effects, constraints) =
  let* typed_effects =
    effects
    |> List.map
         (type_check_effect ~lhs_type_env ~args_scope ~rhs_scope ~fun_env)
    |> all_ok
  in
  let* typed_constraints =
    constraints
    |> List.map (type_check_bool ~scope:constr_scope ~fun_env)
    |> all_ok
  in
  Ok (typed_effects, typed_constraints)

(** Type-checks a service, by ensuring that:
    - the parameters have unique names;
    - the preconditions are boolean expressions mentioning only the service
      parameters, the global variables and the functions defined in the
      contract;
    - the QoS postcondition is well-formed, i.e. its effects assign only to QoS
      fields or to function using only QoS fields as arguments, the assigned
      expressions are of the correct type and mention only the service
      parameters, the global variables and the QoS fields; its constraints are
      boolean expressions mentioning only the service parameters, the global
      variables and the QoS fields
    - the ok and err postconditions are well-formed, i.e. its effects assign
      only to global variables, the return variable or to function mentioning
      only globals or the service's parameters, and the assigned expressions are
      of the correct type and mention only the service parameters and the global
      variables; its constraints are boolean expressions mentioning only the
      return variable, the service's parameters or the global variables. *)
let type_check_service ~globals_env ~qos_env ~fun_env { it = s } =
  let* params_env =
    build_env
      ~pp_err:(fun fmt x -> Fmt.pf fmt "Duplicate parameter name '%s'" x)
      s.params
  in
  let return_name, return_type = s.returns in
  let return_env = StringMap.singleton return_name return_type in
  (* Preconditions and effect's rhs have access to the service parameters and the global variables *)
  let base_scope = [ params_env; globals_env ] in
  (* QoS postconditions have also access to the QoS fields *)
  let qos_scope = qos_env :: base_scope in
  (* Constraints in ok and err postconditions have also access to the return variable *)
  let return_scope = return_env :: base_scope in
  (* Effects in ok and err postconditions may assign to a global variable or to the return variable *)
  let global_or_return_env =
    StringMap.union (fun _ t _ -> Some t) return_env globals_env
  in
  let* typed_precond =
    s.precond |> List.map (type_check_bool ~scope:base_scope ~fun_env) |> all_ok
  in
  let* typed_qos_postcond =
    type_check_postcond ~lhs_type_env:qos_env ~args_scope:qos_scope
      ~rhs_scope:qos_scope ~constr_scope:qos_scope ~fun_env s.qos_postcond
  in
  let* typed_ok_postcond =
    type_check_postcond ~lhs_type_env:global_or_return_env
      ~args_scope:base_scope ~rhs_scope:base_scope ~constr_scope:return_scope
      ~fun_env s.ok_postcond
  in
  let* typed_err_postcond =
    match s.err_postcond with
    | None -> Ok None
    | Some postcond ->
        let* typed_postcond =
          type_check_postcond ~lhs_type_env:global_or_return_env
            ~args_scope:base_scope ~rhs_scope:base_scope
            ~constr_scope:return_scope ~fun_env postcond
        in
        Ok (Some typed_postcond)
  in
  Ok
    TC.
      {
        name = s.name;
        params = s.params |> List.map drop_loc |> List.map fst;
        returns = s.returns;
        precond = typed_precond;
        qos_postcond = typed_qos_postcond;
        ok_postcond = typed_ok_postcond;
        err_postcond = typed_err_postcond;
      }

let type_check_contract c =
  let* services_names =
    c.services
    |> List.map (fun { it = s; at } -> { it = s.name; at })
    |> build_set ~pp_err:(fun fmt name ->
        Fmt.pf fmt "Duplicate service name '%s'" name)
  in
  let* globals_env =
    build_env
      ~pp_err:(fun fmt x -> Fmt.pf fmt "Duplicate global variable name '%s'" x)
      c.globals
  in
  let* qos_env =
    build_env
      ~pp_err:(fun fmt x -> Fmt.pf fmt "Duplicate QoS field name '%s'" x)
      c.qos
  in
  let qos_fields =
    qos_env |> StringMap.bindings |> List.map fst |> StringSet.of_list
  in
  let* fun_env =
    build_env
      ~pp_err:(fun fmt x -> Fmt.pf fmt "Duplicate function name '%s'" x)
      c.functions
  in
  let* assumptions =
    c.globals_assumptions |> List.map without_fun_app |> all_ok
  in
  let* typed_assumptions =
    assumptions
    |> List.map
         (type_check_bool ~scope:[ globals_env ] ~fun_env:StringMap.empty)
    |> all_ok
  in
  let* typed_services =
    c.services
    |> List.map (type_check_service ~globals_env ~qos_env ~fun_env)
    |> all_ok
  in
  let* typed_policies =
    c.policies
    |> List.map (type_check_policy ~services_names ~qos_fields)
    |> all_ok
  in
  Ok
    TC.
      {
        globals = c.globals |> List.map drop_loc;
        globals_assumptions = typed_assumptions;
        functions = c.functions |> List.map drop_loc |> List.map fst;
        qos = c.qos |> List.map drop_loc;
        policies = typed_policies;
        services = typed_services;
      }
