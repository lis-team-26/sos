open OrchestratorAST
module T = TypedOrchestratorAST
open Expr.AST
open Expr.TypeChecker
open Contract.AST
open Utils.Data
open Utils.Loc
open Utils.Result
open Utils.Scope
open Utils.Types
open Utils.Types_pp
open Result.Syntax

(** Maps each service name to its signature, consisting of its parameter types
    and return type. *)
let build_svc_env services =
  let service_signature s =
    (s.name, (List.map (drop_loc >> snd) s.params, snd s.returns))
  in
  services |> List.map service_signature |> StringMap.of_list

(** Type-checks a service invocation: checks the service exists and that the
    arguments match its parameter types, returning the service's return type
    together with the type-checked arguments. *)
let type_check_invoke ~scope ~fun_env ~svc_env ~loc svc args =
  let* params_types, ret_type =
    match StringMap.find_opt svc svc_env with
    | None -> located_error ~loc "Service %s not found" svc
    | Some (params_types, ret_type) -> Ok (params_types, ret_type)
  in
  if List.length params_types <> List.length args then
    located_error ~loc "Service %s expects %d arguments but got %d" svc
      (List.length params_types) (List.length args)
  else
    let* typed_args =
      List.map2 (type_check_expr ~scope ~fun_env) params_types args |> all_ok
    in
    Ok (ret_type, typed_args)

(** Type-checks a statement against [scope], returning the typed statement and
    the scope holding after it. A statement may extend the scope (a declaration
    introduces a variable); control-flow branches get their own scope so their
    declarations do not escape. A variable used or assigned before being
    declared is not in scope, and therefore reported as an error. *)
let rec type_check_stmt ~scope ~fun_env ~qos_env ~svc_env stmt =
  let loc = stmt.at in
  let* typed_stmt, scope =
    match stmt.it with
    | Skip -> Ok (T.Skip, scope)
    | Declare (t, x, e) -> (
        match lookup x [ List.hd scope ] with
        | Some _ ->
            located_error ~loc "Variable %s already declared in this scope" x
        | None ->
            let* typed_e = type_check_expr ~scope ~fun_env t e in
            let scope = declare x t scope in
            Ok (T.Declare (x, typed_e), scope))
    | Assign (x, e) -> (
        match lookup x scope with
        | None -> located_error ~loc "Variable %s assigned before declaration" x
        | Some t ->
            let* typed_e = type_check_expr ~scope ~fun_env t e in
            Ok (T.Assign (x, typed_e), scope))
    | Assume e ->
        let* typed_e = type_check_bool ~scope ~fun_env e in
        Ok (T.Assume typed_e, scope)
    | Assert e ->
        let* typed_e = type_check_bool ~scope ~fun_env e in
        Ok (T.Assert typed_e, scope)
    | Seq (s1, s2) ->
        let* typed_s1, scope =
          type_check_stmt ~scope ~fun_env ~qos_env ~svc_env s1
        in
        let* typed_s2, scope =
          type_check_stmt ~scope ~fun_env ~qos_env ~svc_env s2
        in
        Ok (T.Seq (typed_s1, typed_s2), scope)
    | If (c, s1, s2) ->
        let* typed_c = type_check_bool ~scope ~fun_env c in
        let* typed_s1, _ =
          type_check_stmt ~scope:(push_env scope) ~fun_env ~qos_env ~svc_env s1
        in
        let* typed_s2, _ =
          type_check_stmt ~scope:(push_env scope) ~fun_env ~qos_env ~svc_env s2
        in
        Ok (T.If (typed_c, typed_s1, typed_s2), scope)
    | While (c, body) ->
        let* typed_c = type_check_bool ~scope ~fun_env c in
        let* typed_body, _ =
          type_check_stmt ~scope:(push_env scope) ~fun_env ~qos_env ~svc_env
            body
        in
        Ok (T.While (typed_c, typed_body), scope)
    | Invoke (svc, args) ->
        let* _, typed_args =
          type_check_invoke ~scope ~fun_env ~svc_env ~loc svc args
        in
        Ok (T.Invoke (svc, typed_args), scope)
    | DeclareInvoke (x, svc, args) -> (
        match lookup x [ List.hd scope ] with
        | Some _ ->
            located_error ~loc "Variable %s already declared in this scope" x
        | None ->
            let* ret_type, args' =
              type_check_invoke ~scope ~fun_env ~svc_env ~loc svc args
            in
            let t = TReceipt { ret_type; qos_types = qos_env } in
            let scope = declare x t scope in
            Ok (T.DeclareInvoke (x, svc, args'), scope))
    | AssignInvoke (x, svc, args) -> (
        let* ret_type, typed_args =
          type_check_invoke ~scope ~fun_env ~svc_env ~loc svc args
        in
        match lookup x scope with
        | None -> located_error ~loc "Variable %s assigned before declaration" x
        | Some (TReceipt { ret_type }) ->
            Ok (T.AssignInvoke (x, svc, typed_args), scope)
        | Some t ->
            located_error ~loc "Variable %s expected receipt but found %a" x
              pp_var_type t)
  in
  Ok (located ~loc typed_stmt, scope)

let type_check_orchestrator contract program =
  let globals = List.map drop_loc contract.globals in
  let fun_env = contract.functions |> List.map drop_loc |> StringMap.of_list in
  let svc_env = contract.services |> List.map drop_loc |> build_svc_env in
  let qos_env = contract.qos |> List.map drop_loc |> StringMap.of_list in
  let* typed_stmt, _ =
    type_check_stmt
      ~scope:[ StringMap.of_list globals ]
      ~fun_env ~qos_env ~svc_env program
  in
  Ok typed_stmt
