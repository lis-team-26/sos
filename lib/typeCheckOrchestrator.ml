open Utils.Data
open Expr.AST
open Contract.AST
open Orchestrator.AST
open TypeCheckExpr
module T = TypedOrchestrator.AST

let ( let* ) = Result.bind

(* Renders a type for use in error messages. *)
let string_of_var_type = function TInt -> "int" | TBool -> "bool"

(* Maps each service name to its parameter types and its return type. *)
let service_map_of_services services =
  env_of_list
    (List.map
       (fun (s : service) -> (s.name, (List.map snd s.params, snd s.returns)))
       services)

(* Resolves a service invocation: checks the service exists and that the
   arguments match its parameter types, returning the service's return type
   together with the type-checked arguments. *)
let type_check_invoke svc service_map scope fn_map args =
  match StringMap.find_opt svc service_map with
  | None -> Error (Fmt.str "Service %s not found" svc)
  | Some (params_types, ret_type) ->
      let* typed_args = type_check_args svc params_types args scope fn_map in
      Ok (ret_type, typed_args)

(* Type-checks a statement against [scope], returning the typed statement and
   the scope holding after it. A statement may extend the scope (a declaration
   introduces a variable); control-flow branches get their own scope so their
   declarations do not escape. A variable used or assigned before being
   declared is not in scope, and therefore reported as an error. *)
let rec type_check_stmt fn_map service_map scope = function
  | Skip -> Ok (T.Skip, scope)
  | Declare (t, x, e) ->
      let* typed_e = type_check_expr t scope fn_map e in
      let* scope = declare x t scope in
      Ok (T.Assign (x, typed_e), scope)
  | Assign (x, e) -> (
      match lookup x scope with
      | None -> Error (Fmt.str "Variable %s assigned before declaration" x)
      | Some t ->
          let* typed_e = type_check_expr t scope fn_map e in
          Ok (T.Assign (x, typed_e), scope))
  | Assume e ->
      let* typed_e = type_check_bool scope fn_map e in
      Ok (T.Assume typed_e, scope)
  | Assert e ->
      let* typed_e = type_check_bool scope fn_map e in
      Ok (T.Assert typed_e, scope)
  | Seq (s1, s2) ->
      let* typed_s1, scope = type_check_stmt fn_map service_map scope s1 in
      let* typed_s2, scope = type_check_stmt fn_map service_map scope s2 in
      Ok (T.Seq (typed_s1, typed_s2), scope)
  | If (c, s1, s2) ->
      let* typed_c = type_check_bool scope fn_map c in
      let* typed_s1, _ =
        type_check_stmt fn_map service_map (push_scope scope) s1
      in
      let* typed_s2, _ =
        type_check_stmt fn_map service_map (push_scope scope) s2
      in
      Ok (T.If (typed_c, typed_s1, typed_s2), scope)
  | While (c, body) ->
      let* typed_c = type_check_bool scope fn_map c in
      let* typed_body, _ =
        type_check_stmt fn_map service_map (push_scope scope) body
      in
      Ok (T.While (typed_c, typed_body), scope)
  | Invoke (svc, args) ->
      let* _, typed_args =
        type_check_invoke svc service_map scope fn_map args
      in
      Ok (T.Invoke (svc, typed_args), scope)
  | AssignInvoke (x, svc, args) -> (
      match lookup x scope with
      | None -> Error (Fmt.str "Variable %s assigned before declaration" x)
      | Some t ->
          let* ret_type, typed_args =
            type_check_invoke svc service_map scope fn_map args
          in
          if t <> ret_type then
            Error
              (Fmt.str "Service %s returns %s but variable %s has type %s" svc
                 (string_of_var_type ret_type)
                 x (string_of_var_type t))
          else Ok (T.AssignInvoke (x, svc, typed_args), scope))
  | DeclareInvoke (t, x, svc, args) ->
      let* ret_type, args' =
        type_check_invoke svc service_map scope fn_map args
      in
      if t <> ret_type then
        Error
          (Fmt.str "Service %s returns %s but variable %s is declared as %s" svc
             (string_of_var_type ret_type)
             x (string_of_var_type t))
      else
        let* scope = declare x t scope in
        Ok (T.AssignInvoke (x, svc, args'), scope)

(* Type-checks an orchestrator against a contract, which supplies the function
   signatures (for calls inside expressions), the service signatures (for
   invocations) and the globals that are in scope from the start. *)
let type_check_orchestrator (c : contract) stmt =
  let fn_map = env_of_list c.functions in
  let service_map = service_map_of_services c.services in
  let* typed_stmt, _ =
    type_check_stmt fn_map service_map [ env_of_list c.globals ] stmt
  in
  Ok typed_stmt
