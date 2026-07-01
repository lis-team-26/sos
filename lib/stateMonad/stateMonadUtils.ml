open StateMonadCore
open Symbolic.Runtime
open Soteria.Symex.Fuel_gauge.Fuel_value
open Utils.Data
open Utils.Scope

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

let map_error ~last_ok err_state =
  Symex.Result.map_error err_state (error_from_cause ~last_ok)

let lift_expr_monad m =
 fun (state, policy_checkers) ->
  let++ v, function_envs = m state.function_envs |> map_error ~last_ok:state in
  (v, ({ state with function_envs }, policy_checkers))

let scoped m =
 fun (state, policy_checkers) ->
  let++ (), (state, policy_checkers) =
    m ({ state with scope = push_env state.scope }, policy_checkers)
  in
  ((), ({ state with scope = pop_env state.scope }, policy_checkers))

let branch b then_m else_m =
  let open StatementMonad in
  fun state ->
    if (fst state).fuel.branching = Fuel.Infinite then
      if%sat b then then_m () state else else_m () state
    else
      let* b_sat = Symex.map (Symex.assert_ (Symex.Value.not b)) not in
      let* not_b_sat = Symex.map (Symex.assert_ b) not in
      match (b_sat, not_b_sat) with
      | true, true ->
          if%sat b then then_m () state
          else
            (let& () = consume_branching_fuel 1 in
             else_m ())
              state
      | true, false -> then_m () state
      | false, true -> else_m () state
      | false, false -> failwith "Unreachable: both branches are unsatisfiable"

let ( let&&* ) m f = StatementMonad.bind (lift_expr_monad m) f
let ( let&&+ ) m f = StatementMonad.map (lift_expr_monad m) f
