open OrchestratorAST
module T = TypedOrchestratorAST
open Expr.AST
open Expr.TypeCheck
open Contract.AST
open Utils.Data
open Utils.Types
open Utils.Types_pp

let ( let* ) = Result.bind

(* Maps each service name to its parameter types and its return type. *)
let service_map_of_services services =
  services
  |> List.map (fun s -> (s.name, (List.map snd s.params, snd s.returns)))
  |> StringMap.of_list

(* Resolves a service invocation: checks the service exists and that the
   arguments match its parameter types, returning the service's return type
   together with the type-checked arguments. *)
let type_check_invoke svc service_map scope fun_map args =
  match StringMap.find_opt svc service_map with
  | None -> Error (Fmt.str "Service %s not found" svc)
  | Some (params_types, ret_type) ->
      let* typed_args = type_check_args svc params_types args scope fun_map in
      Ok (ret_type, typed_args)

(* Type-checks a statement against [scope], returning the typed statement and
   the scope holding after it. A statement may extend the scope (a declaration
   introduces a variable); control-flow branches get their own scope so their
   declarations do not escape. A variable used or assigned before being
   declared is not in scope, and therefore reported as an error. *)
let rec type_check_stmt fun_map svc_map qos_map scope = function
  | Skip -> Ok (T.Skip, scope)
  | Declare (t, x, e) -> (
      match lookup x [ List.hd scope ] with
      | Some _ -> Error (Fmt.str "Variable %s already declared in this scope" x)
      | None ->
          let* typed_e = type_check_expr t scope fun_map e in
          let scope = declare x t scope in
          Ok (T.Declare (x, typed_e), scope))
  | Assign (x, e) -> (
      match lookup x scope with
      | None -> Error (Fmt.str "Variable %s assigned before declaration" x)
      | Some t ->
          let* typed_e = type_check_expr t scope fun_map e in
          Ok (T.Assign (x, typed_e), scope))
  | Assume e ->
      let* typed_e = type_check_bool scope fun_map e in
      Ok (T.Assume typed_e, scope)
  | Assert (e, loc) ->
      let* typed_e = type_check_bool scope fun_map e in
      Ok (T.Assert (typed_e, loc), scope)
  | Seq (s1, s2) ->
      let* typed_s1, scope = type_check_stmt fun_map svc_map qos_map scope s1 in
      let* typed_s2, scope = type_check_stmt fun_map svc_map qos_map scope s2 in
      Ok (T.Seq (typed_s1, typed_s2), scope)
  | If (c, s1, s2) ->
      let* typed_c = type_check_bool scope fun_map c in
      let* typed_s1, _ =
        type_check_stmt fun_map svc_map qos_map (push_env scope) s1
      in
      let* typed_s2, _ =
        type_check_stmt fun_map svc_map qos_map (push_env scope) s2
      in
      Ok (T.If (typed_c, typed_s1, typed_s2), scope)
  | While (c, body) ->
      let* typed_c = type_check_bool scope fun_map c in
      let* typed_body, _ =
        type_check_stmt fun_map svc_map qos_map (push_env scope) body
      in
      Ok (T.While (typed_c, typed_body), scope)
  | Invoke (svc, args, loc) ->
      let* _, typed_args = type_check_invoke svc svc_map scope fun_map args in
      Ok (T.Invoke (svc, typed_args, loc), scope)
  | DeclareInvoke (x, svc, args, loc) -> (
      match lookup x [ List.hd scope ] with
      | Some _ -> Error (Fmt.str "Variable %s already declared in this scope" x)
      | None ->
          let* ret_type, args' =
            type_check_invoke svc svc_map scope fun_map args
          in
          let t = TReceipt { ret_type; qos_types = qos_map } in
          let scope = declare x t scope in
          Ok (T.DeclareInvoke (x, svc, args', loc), scope))
  | AssignInvoke (x, svc, args, loc) -> (
      let* ret_type, typed_args =
        type_check_invoke svc svc_map scope fun_map args
      in
      match lookup x scope with
      | None -> Error (Fmt.str "Variable %s assigned before declaration" x)
      | Some (TReceipt { ret_type }) ->
          Ok (T.AssignInvoke (x, svc, typed_args, loc), scope)
      | Some t ->
          Error
            (Fmt.str "Variable %s expected receipt but found %a" x pp_var_type t)
      )

(* Type-checks an orchestrator against a contract, which supplies the function
   signatures (for calls inside expressions), the service signatures (for
   invocations) and the globals that are in scope from the start. *)
let type_check_orchestrator contract program =
  let fun_map = StringMap.of_list contract.functions in
  let svc_map = service_map_of_services contract.services in
  let qos_map = StringMap.of_list contract.qos in
  let* typed_program, _ =
    type_check_stmt fun_map svc_map qos_map
      [ StringMap.of_list contract.globals ]
      program
  in
  Ok typed_program
