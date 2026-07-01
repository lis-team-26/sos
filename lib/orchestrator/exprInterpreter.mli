open Expr.TypedAST
open Symbolic.Data
open Utils.Scope

val symb_eval_aexpr :
  scope:symbolic_value scope ->
  aexpr ->
  (symb_int, 'fix) StateMonad.ExpressionMonad.t
(** Evaluates an arithmetic expression symbolically in the given scope,
    producing a state-monadic computation which returns a symbolic integer. See
    [StateMonad.ExpressionMonad] for details. *)

val symb_eval_bexpr :
  scope:symbolic_value scope ->
  bexpr ->
  (symb_bool, 'fix) StateMonad.ExpressionMonad.t
(** Evaluates a boolean expression symbolically in the given scope, producing a
    state-monadic computation which returns a symbolic boolean. See
    [StateMonad.ExpressionMonad] for details. *)

val symb_eval_expr :
  scope:symbolic_value scope ->
  typed_expr ->
  (symbolic_value, 'fix) StateMonad.ExpressionMonad.t
(** Evaluates a typed expression (either arithmetic or boolean) symbolically in
    the given scope, producing a state-monadic computation which returns a
    symbolic value. See [StateMonad.ExpressionMonad] for details. *)
