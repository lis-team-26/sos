open OrchestratorAST
open Contract.AST
open Symbolic.Runtime
open PolicyChecker

val build_symb_process :
  stmt -> contract -> pChecker list -> (ok_state, err_state, 'a) Symex.Result.t
