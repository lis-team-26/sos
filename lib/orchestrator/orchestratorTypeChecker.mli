open Utils.Loc

val type_check_orchestrator :
  Contract.AST.contract ->
  OrchestratorAST.stmt ->
  (TypedOrchestratorAST.stmt, string located) result
(** Type-checks an orchestrator against a contract, which supplies the function
    signatures (for calls inside expressions), the service signatures (for
    invocations) and the globals that are in scope from the start. *)
