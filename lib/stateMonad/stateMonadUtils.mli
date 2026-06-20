open StateMonadCore
open Symbolic.Runtime
open PolicyChecker
open Utils.Data

val map_error :
  ok_state ->
  ('a, error_cause, 'fix) Symex.Result.t ->
  loc:loc ->
  ('a, not_ok_state, 'fix) Symex.Result.t

val lift_fm : ('a, 'fix) FunctionalMonad.t -> ('a, 'fix) OkStateMonad.t
val scoped : (unit, 'fix) OkStateMonad.t -> (unit, 'fix) OkStateMonad.t

val branch :
  Symex.Value.sbool Symex.Value.t ->
  (unit, 'fix) OkStateMonad.t ->
  (unit, 'fix) OkStateMonad.t ->
  (unit, 'fix) OkStateMonad.t

val get_state : (ok_state, 'fix) OkStateMonad.t
val get_policy_checkers : (policyChecker list, 'fix) OkStateMonad.t
val modify_state : (ok_state -> ok_state) -> (unit, 'fix) OkStateMonad.t

val modify_policy_checkers :
  (policyChecker list -> policyChecker list) -> (unit, 'fix) OkStateMonad.t

val consume_steps_fuel : int -> (unit, 'fix) OkStateMonad.t
val consume_branching_fuel : int -> (unit, 'fix) OkStateMonad.t
val consume_unroll_fuel : int -> (unit, 'fix) OkStateMonad.t

val ( let&&* ) :
  ('a, 'fix) FunctionalMonad.t ->
  ('a -> ('c, 'fix) OkStateMonad.t) ->
  ('c, 'fix) OkStateMonad.t

val ( let&&+ ) :
  ('a, 'fix) FunctionalMonad.t -> ('a -> 'c) -> ('c, 'fix) OkStateMonad.t
