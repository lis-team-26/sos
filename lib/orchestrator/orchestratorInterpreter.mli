open TypedOrchestratorAST
open Contract.TypedAST
open Symbolic.Runtime
open PolicyChecker

val build_symb_process :
  stmt ->
  contract ->
  policyChecker list ->
  (ok_state, err_state, 'a) Symex.Result.t
