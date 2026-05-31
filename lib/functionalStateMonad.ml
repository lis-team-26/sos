open Utils.Data
open Symbolic.Data
open Symbolic.Runtime

module FSM = struct
  type ('ok, 'err, 'fix) t =
    function_map -> ('ok * function_map, 'err, 'fix) Symex.Result.t

  let return x = fun function_map -> Symex.Result.ok (x, function_map)

  let bind m f =
   fun function_map ->
    let** x, function_map' = m function_map in
    f x function_map'

  let lift_symex_result m f =
   fun function_map ->
    let* x = m in
    f x function_map

  let lift_symex m f =
   fun function_map ->
    let++ x = m in
    (f x, function_map)

  let run m f state =
    let** v, function_map = m state.function_map in
    f (v, { state with function_map })

  let ( let& ) = bind
  let ( let&* ) = lift_symex_result
  let ( let&+ ) = lift_symex

  let get_function_map =
   fun (function_map : function_map) ->
    Symex.Result.ok (function_map, function_map)

  let set_function_map function_map = fun _ -> Symex.Result.ok ((), function_map)

  let modify_function_map f =
   fun function_map ->
    let function_map' = f function_map in
    Symex.Result.ok ((), function_map')

  let fold_list xs ~init ~f =
    List.fold_left
      (fun acc x fm ->
        let** v, fm' = acc fm in
        f v x fm')
      (return init) xs
end
