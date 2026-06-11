open Symbolic.Runtime
open Soteria.Symex
open Utils.Data

module type S = sig
  type ok
  type err
end

module Make (S : S) = struct
  type ok = S.ok
  type err = S.err
  type ('a, 'fix) t = ok -> ('a * ok, err, 'fix) Symex.Result.t

  let return x = fun state -> Symex.Result.ok (x, state)

  let bind m f =
   fun state ->
    let** x, state' = m state in
    f x state'

  let map m f =
   fun state ->
    let++ x, state' = m state in
    (f x, state')

  let run m state = m state

  let run_unit m state =
    let++ (), state' = m state in
    state'

  let get = fun state -> Symex.Result.ok (state, state)
  let put state = fun _ -> Symex.Result.ok ((), state)
  let modify f = fun state -> Symex.Result.ok ((), f state)

  let lift_symex_result m =
   fun state ->
    let++ x = m in
    (x, state)

  let lift_symex m =
   fun state ->
    let* x = m in
    Symex.Result.ok (x, state)

  let fold_list xs ~init ~f =
    List.fold_left (fun acc x -> bind acc (fun acc -> f acc x)) (return init) xs

  let map_list xs ~f =
    let mapped =
      fold_list xs ~init:[] ~f:(fun acc x ->
          bind (f x) (fun y -> return (y :: acc)))
    in
    bind mapped (fun ys -> return (List.rev ys))

  let ( let& ) = bind
  let ( let&* ) m f = bind (lift_symex m) f
  let ( let&** ) m f = bind (lift_symex_result m) f
  let ( let&+ ) m f = map (lift_symex m) f
  let ( let&++ ) m f = map (lift_symex_result m) f
end

module FunctionalMonad = Make (struct
  type ok = function_envs
  type err = string
end)

module OkStateMonad = Make (struct
  type ok = ok_state
  type err = err_state
end)

let lift_fm m =
  let map_error old_ok_state err_state =
    Symex.Result.map_error err_state (fun msg ->
        { msg; err_stack = old_ok_state.ok_stack })
  in
  fun state ->
    let++ v, function_envs = m state.function_envs |> map_error state in
    (v, { state with function_envs })

let scoped m =
 fun state ->
  let++ (), state = m { state with scope = push_scope state.scope } in
  ((), { state with scope = pop_scope state.scope })

let branch b then_m else_m =
 fun state -> if%sat b then then_m state else else_m state
