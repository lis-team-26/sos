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
  Symex.Result.map_error err_state (fun cause ->
      located_error_cause cause ~loc |> error_from_cause old_ok_state)

let lift_fm m =
 fun (state, policy_checkers) ->
  let++ v, function_envs =
    m state.function_envs |> map_error state ~loc:NoLoc
  in
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
    let* b_sat = Symex.map (Symex.assert_ (Symex.Value.not b)) not in
    let* not_b_sat = Symex.map (Symex.assert_ b) not in
    match (b_sat, not_b_sat) with
    | true, true ->
        if%sat b then then_m state
        else
          (let& () = consume_branching_fuel 1 in
           else_m)
            state
    | true, false -> then_m state
    | false, true -> else_m state
    | false, false -> failwith "Unreachable: both branches are unsatisfiable"

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
