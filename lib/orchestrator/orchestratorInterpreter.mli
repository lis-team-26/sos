open TypedOrchestratorAST
open Contract.TypedAST
open Symbolic.Data
open Symbolic.Runtime
open PolicyChecker
open Utils.Data

val build_symb_process :
  stmt -> contract -> (ok_state, err_state, 'a) Symex.Result.t
