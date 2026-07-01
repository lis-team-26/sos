open TypedOrchestratorAST
open Contract.TypedAST
open Symbolic.Runtime

val build_symex_process :
  fuel:fuel -> contract -> stmt -> (ok_state, not_ok_state, 'fix) Symex.Result.t
(** Builds a [Symbolic.Runtime.Symex] symbolic computation evaluating the given
    orchestrator statement against the given contract, limited by the given
    fuel. *)
