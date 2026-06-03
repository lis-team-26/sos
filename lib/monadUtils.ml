open Symbolic.Runtime
open Soteria.Symex

module FunctionalMonad = MonadState.Make (struct
  type ok = function_map
  type err = ok_monad_state -> err_monad_state
end)

module OkStateMonad = MonadState.Make (struct
  type ok = ok_monad_state
  type err = err_monad_state
end)

let seal_error ok partial_err =
  Symex.Result.map_error partial_err (fun partial_err -> partial_err ok)

let lift_fm m =
 fun state ->
  let++ v, function_map = m state.function_map |> seal_error state in
  (v, { state with function_map })

let branch b then_m else_m =
 fun state -> if%sat b then then_m state else else_m state