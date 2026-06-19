open ContractAST
module TC = TypedContractAST
open Expr.AST
module TE = Expr.TypedAST
open Expr.TypeCheck
open Utils.Data

let ( let* ) = Result.bind

(* Builds a variable-type environment from a list of typed variables. *)
let build_static_env vars =
  List.fold_left (fun acc (x, t) -> StringMap.add x t acc) StringMap.empty vars

(* Builds the function-signature map consulted by the expression type checker. *)
let build_static_fun_map functions =
  List.fold_left
    (fun acc (f, ft) -> StringMap.add f ft acc)
    StringMap.empty functions

(* Comparison binary operators are the only ones allowed in a QoS policy. *)
let cmp_op_of_bin_op = function
  | Eq -> Ok TE.Eq
  | Neq -> Ok TE.Neq
  | Lt -> Ok TE.Lt
  | Le -> Ok TE.Le
  | Gt -> Ok TE.Gt
  | Ge -> Ok TE.Ge
  | Add | Sub | Mul | Div | And | Or ->
      Error "QoS policy must use a comparison operator"

let type_check_policy (policy_type, group_by) =
  match policy_type with
  | QosFieldOp (aggr, field, op, n) ->
      let* cmp = cmp_op_of_bin_op op in
      Ok (TC.QosFieldOp (aggr, field, cmp, n), group_by)
  | Regex (s2letter, regex) -> Ok (TC.Regex (s2letter, regex), group_by)
  | Sort field -> Ok (TC.Sort field, group_by)

(* Type-checks a single effect [lhs := rhs].
   - [lhs_type] resolves the declared type of an assigned variable (LVar);
   - [args_scope] type-checks the arguments of a function-application lhs;
   - [rhs_scope] type-checks the right-hand side, against the lhs type.

   The typed contract AST keeps effect expressions untyped (see [effct] in
   [TypedContractAST]), so the original expressions are returned unchanged once
   they have been validated. *)
let type_check_effect lhs_type_env args_scope rhs_scope static_fun_map (lhs, rhs)
    =
  match lhs with
  | LVar v -> (
      match lhs_type_env v with
      | None -> Error (Fmt.str "Effect assigns to unknown variable %s" v)
      | Some t ->
          let* typed_rhs = type_check_expr t rhs_scope static_fun_map rhs in
          Ok (TC.LVar v, typed_rhs))
  | LApp (f, args) -> (
      match StringMap.find_opt f static_fun_map with
      | None -> Error (Fmt.str "Effect applies unknown function %s" f)
      | Some (TFun (params_types, ret_type)) ->
          let* typed_args =
            type_check_args f params_types args args_scope static_fun_map
          in
          let* typed_rhs =
            type_check_expr ret_type rhs_scope static_fun_map rhs
          in
          Ok (TC.LApp (f, typed_args), typed_rhs))

(* Type-checks a postcondition: its effects and its boolean constraints. *)
let type_check_postcond lhs_type_env args_scope rhs_scope constr_scope
    static_fun_map (effects, constraints) =
  let* effects' =
    sequence_results
      (List.map
         (type_check_effect lhs_type_env args_scope rhs_scope static_fun_map)
         effects)
  in
  let* constraints' =
    sequence_results
      (List.map (type_check_bool constr_scope static_fun_map) constraints)
  in
  Ok (effects', constraints')

let type_check_service globals_env qos_env static_fun_map (s : service) =
  let params_env = build_static_env s.params in
  let return_name, return_type = s.returns in
  let return_env = StringMap.singleton return_name return_type in
  (* Preconditions and effect right-hand sides see the service parameters and
     the contract globals. *)
  let base_scope = [ params_env; globals_env ] in
  (* QoS postconditions additionally see the QoS fields. *)
  let qos_scope = qos_env :: base_scope in
  (* OK/error postcondition constraints additionally see the return variable. *)
  let return_scope = return_env :: base_scope in
  (* OK/error effects may assign to a global or to the return variable. *)
  let global_or_return_env v =
    match StringMap.find_opt v globals_env with
    | Some _ as t -> t
    | None -> StringMap.find_opt v return_env
  in
  let typed =
    let* typed_precond =
      sequence_results
        (List.map (type_check_bool base_scope static_fun_map) s.precond)
    in
    let* typed_qos_postcond =
      type_check_postcond
        (fun x -> StringMap.find_opt x qos_env)
        qos_scope qos_scope qos_scope static_fun_map s.qos_postcond
    in
    let* typed_ok_postcond =
      type_check_postcond global_or_return_env base_scope base_scope
        return_scope static_fun_map s.ok_postcond
    in
    let* typed_err_postcond =
      match s.err_postcond with
      | None -> Ok None
      | Some pc ->
          let* typed_pc =
            type_check_postcond global_or_return_env base_scope base_scope
              return_scope static_fun_map pc
          in
          Ok (Some typed_pc)
    in
    Ok
      TC.
        {
          name = s.name;
          params = List.map fst s.params;
          returns = s.returns;
          precond = typed_precond;
          qos_postcond = typed_qos_postcond;
          ok_postcond = typed_ok_postcond;
          err_postcond = typed_err_postcond;
        }
  in
  Result.map_error (fun msg -> Fmt.str "Service %s: %s" s.name msg) typed

let type_check_contract (c : contract) =
  let globals_env = build_static_env c.globals in
  let qos_env = build_static_env c.qos in
  let static_fun_map = build_static_fun_map c.functions in
  let* typed_services =
    sequence_results
      (List.map
         (type_check_service globals_env qos_env static_fun_map)
         c.services)
  in
  let* typed_policies =
    sequence_results (List.map type_check_policy c.policies)
  in
  Ok
    TC.
      {
        globals = c.globals;
        functions = List.map fst c.functions;
        policies = typed_policies;
        qos = c.qos;
        services = typed_services;
      }
