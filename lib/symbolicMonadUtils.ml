open Symbolic.Runtime
open Soteria.Symex
open Utils.Data

module FunctionalMonad = SymbolicMonadState.Make (struct
  type ok = function_envs
  type err = string
end)

module OkStateMonad = SymbolicMonadState.Make (struct
  type ok = ok_state
  type err = err_state
end)

let seal_error (old_ok_state : ok_state) err_state =
  Symex.Result.map_error err_state (fun msg ->
      { msg; stack = old_ok_state.stack })

let lift_fm m =
 fun state ->
  let++ v, function_envs = m state.function_envs |> seal_error state in
  (v, { state with function_envs })

let scoped m =
 fun state ->
  let++ (), state = m { state with env = push_scope state.env } in
  ((), { state with env = pop_scope state.env })

let branch b then_m else_m =
 fun state -> if%sat b then then_m state else else_m state
