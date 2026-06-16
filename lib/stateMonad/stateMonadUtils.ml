open StateMonadCore
open Symbolic.Runtime
open Soteria.Symex.Fuel_gauge.Fuel_value
open Utils.Data

let consume_steps_fuel n =
 fun (state, policy_checkers) ->
  match state.fuel.steps with
  | Finite 0 -> Symex.Result.error (Unexplored state)
  | steps_fuel ->
      let fuel = { state.fuel with steps = decrease steps_fuel n } in
      Symex.Result.ok ((), ({ state with fuel }, policy_checkers))

let consume_branching_fuel n =
 fun (state, policy_checkers) ->
  match state.fuel.branching with
  | Finite 0 -> Symex.Result.error (Unexplored state)
  | branching_fuel ->
      let fuel = { state.fuel with branching = decrease branching_fuel n } in
      Symex.Result.ok ((), ({ state with fuel }, policy_checkers))

let consume_unroll_fuel n =
 fun (state, policy_checkers) ->
  match state.fuel.unroll with
  | Finite 0 -> Symex.Result.error (Unexplored state)
  | unroll_fuel ->
      let fuel = { state.fuel with unroll = decrease unroll_fuel n } in
      Symex.Result.ok ((), ({ state with fuel }, policy_checkers))

let map_error old_ok_state err_state ~loc =
  let loc = match loc with Some l -> l | None -> { line = -1; col = -1 } in
  Symex.Result.map_error err_state (fun cause ->
      Err { cause = { value = cause; loc }; err_stack = old_ok_state.ok_stack })

let lift_fm m =
 fun (state, policy_checkers) ->
  let++ v, function_envs = m state.function_envs |> map_error state ~loc:None in
  (v, ({ state with function_envs }, policy_checkers))

let scoped m =
 fun (state, policy_checkers) ->
  let++ (), (state, policy_checkers) =
    m ({ state with scope = push_env state.scope }, policy_checkers)
  in
  ((), ({ state with scope = pop_env state.scope }, policy_checkers))

let branch b then_m else_m =
  let open OkStateMonad in
  fun state ->
    if%sat b then then_m state
    else
      (let& () = consume_branching_fuel 1 in
       else_m)
        state

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

let ( let&&* ) m f = OkStateMonad.bind (lift_fm m) f
let ( let&&+ ) m f = OkStateMonad.map (lift_fm m) f
