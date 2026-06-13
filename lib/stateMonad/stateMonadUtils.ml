open Symbolic.Runtime
open Soteria.Symex
open PolicyChecker
open Utils.Data

let map_error old_ok_state err_state =
  Symex.Result.map_error err_state (fun vid ->
      { vid; err_stack = old_ok_state.ok_stack })

let lift_fm m =
 fun (state, policy_checkers) ->
  let++ v, function_envs = m state.function_envs |> map_error state in
  (v, ({ state with function_envs }, policy_checkers))

let scoped m =
 fun (state, policy_checkers) ->
  let++ (), (state, policy_checkers) =
    m ({ state with scope = push_env state.scope }, policy_checkers)
  in
  ((), ({ state with scope = pop_env state.scope }, policy_checkers))

let branch b then_m else_m =
 fun state -> if%sat b then then_m state else else_m state

let get_state =
 fun (state, policy_checkers) ->
  Symex.Result.ok (state, (state, policy_checkers))

let get_policy_checkers =
 fun (state, policy_checkers) ->
  Symex.Result.ok (policy_checkers, (state, policy_checkers))

let modify_state f =
 fun (state, policy_checkers) ->
  let new_state = f state in
  Symex.Result.ok ((), (new_state, policy_checkers))

let modify_policy_checkers f =
 fun (state, policy_checkers) ->
  let new_policy_checkers = f policy_checkers in
  Symex.Result.ok ((), (state, new_policy_checkers))
