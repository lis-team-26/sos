open TypedOrchestratorAST
open Contract.TypedAST
open Symbolic.Data
open Symbolic.Runtime
open PolicyChecker
open Utils.Data

val build_symex_process :
  stmt -> contract -> fuel -> (ok_state, not_ok_state, 'a) Symex.Result.t
