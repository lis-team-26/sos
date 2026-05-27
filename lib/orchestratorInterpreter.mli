open OrchestratorAST
open Contract.AST
open Expr.AST
open Symbolic.Runtime
open PolicyChecker
open Utils.Data

val build_symb_process :
  stmt ->
  contract ->
  pChecker list ->
  (ok_monad_state, err_monad_state, 'a) Symex.Result.t
