open StateMonadCore
open Symbolic.Runtime
open Soteria.Symex
open PolicyChecker
open Utils.Data

val map_error :
  ok_state ->
  ('a, string, 'fix) Symex.Result.t ->
  ('a, err_state, 'fix) Symex.Result.t

val lift_fm : ('a, 'fix) FunctionalMonad.t -> ('a, 'fix) OkStateMonad.t
val scoped : (unit, 'fix) OkStateMonad.t -> (unit, 'fix) OkStateMonad.t

val branch :
  Symex.Value.sbool Symex.Value.t ->
  (unit, 'fix) OkStateMonad.t ->
  (unit, 'fix) OkStateMonad.t ->
  (unit, 'fix) OkStateMonad.t

val get_state : (ok_state, 'fix) OkStateMonad.t
val get_policy_checkers : (pChecker list, 'fix) OkStateMonad.t
val put_state : ok_state -> (unit, 'fix) OkStateMonad.t
val put_policy_checkers : pChecker list -> (unit, 'fix) OkStateMonad.t
val modify_state : (ok_state -> ok_state) -> (unit, 'fix) OkStateMonad.t

val modify_policy_checkers :
  (pChecker list -> pChecker list) -> (unit, 'fix) OkStateMonad.t
