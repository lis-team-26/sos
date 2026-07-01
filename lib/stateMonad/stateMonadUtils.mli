open StateMonadCore
open Symbolic.Runtime
open PolicyChecker
open Utils.Loc

val get_state : (ok_state, 'fix) StatementMonad.t
(** State-monadic utility to retrieve the current state. *)

val get_policy_checkers : (policy_checker list, 'fix) StatementMonad.t
(** State-monadic utility to retrieve the current list of policy checkers. *)

val modify_state : (ok_state -> ok_state) -> (unit, 'fix) StatementMonad.t
(** State-monadic utility to modify the current state. *)

val modify_policy_checkers :
  (policy_checker list -> policy_checker list) -> (unit, 'fix) StatementMonad.t
(** State-monadic utility to modify the current list of policy checkers. *)

val consume_steps_fuel : int -> (unit, 'fix) StatementMonad.t
(** State-monadic utility to consume fuel for step-based exploration. *)

val consume_branching_fuel : int -> (unit, 'fix) StatementMonad.t
(** State-monadic utility to consume fuel for branching-based exploration. *)

val consume_unroll_fuel : int -> (unit, 'fix) StatementMonad.t
(** State-monadic utility to consume the unroll fuel after evaluating the body
    of a while loop. *)

val map_error :
  last_ok:ok_state ->
  ('a, error_cause located, 'fix) Symex.Result.t ->
  ('a, not_ok_state, 'fix) Symex.Result.t
(** State-monadic utilty used to map a monadic computation (whose error is an
    [error_cause located]) to a monadic computation whose error is a more
    informative [not_ok_state], using teh given cause and the information given
    by the given [last_ok] to fill and complete the [not_ok_state]. *)

val scoped : (unit, 'fix) StatementMonad.t -> (unit, 'fix) StatementMonad.t
(** State-monadic utility taking a statement-monadic computation and lifting it
    to let it be executed in a new scope, which will be popped after the
    computation is done. *)

val branch :
  Symex.Value.sbool Symex.Value.t ->
  (unit -> ('a, 'fix) StatementMonad.t) ->
  (unit -> ('a, 'fix) StatementMonad.t) ->
  ('a, 'fix) StatementMonad.t
(** State-monadic utility for conditional branching. If both branches are
    satisfiable, the else branch will proceed with a decreased branching fuel of
    1. *)

val ( let&&* ) :
  ('a, 'fix) ExpressionMonad.t ->
  ('a -> ('c, 'fix) StatementMonad.t) ->
  ('c, 'fix) StatementMonad.t

val ( let&&+ ) :
  ('a, 'fix) ExpressionMonad.t -> ('a -> 'c) -> ('c, 'fix) StatementMonad.t
