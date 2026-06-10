val type_check_orchestrator :
  Contract.AST.contract ->
  OrchestratorAST.stmt ->
  (TypedOrchestratorAST.stmt, string) result
