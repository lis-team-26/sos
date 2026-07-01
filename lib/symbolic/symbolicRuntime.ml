open SymbolicData
open Expr.TypedAST
open Contract.TypedAST
open Utils.Data
open Utils.Loc
open Utils.Scope
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
module Compo_res = Soteria.Symex.Compo_res
include Symex.Syntax
module Fuel = Soteria.Symex.Fuel_gauge.Fuel_value

(** A map whose keys are symbolic values. It is used to aggregate the history
    during the policy checking for policies whose specification includes a
    'group-by' clause. *)
module SymbolicMap =
  Soteria.Data.S_map.Make
    (Symex)
    (struct
      type t = Typed.T.any Typed.t

      let compare = Typed.compare
      let sem_eq = Typed.sem_eq
      let simplify = Symex.simplify
      let pp = Typed.ppa
      let show v = v |> Typed.Expr.of_value |> Soteria.Tiny_values.Svalue.show
      let distinct = Typed.distinct
    end)

(** A map whose keys are symbolic values. It is used to handle the memoization
    of function applications, in order to be consistent against the functional
    property throughout the execution. *)
module SymbolicListMap =
  Soteria.Data.S_map.Make
    (Symex)
    (struct
      open Typed.Infix

      type t = Typed.T.any Typed.t list

      let compare = List.compare Typed.compare

      let sem_eq l1 l2 =
        if List.length l1 <> List.length l2 then Typed.v_false
        else
          List.combine l1 l2
          |> List.fold_left
               (fun acc (v1, v2) -> acc &&@ Typed.sem_eq v1 v2)
               Typed.v_true

      let simplify = Symex.map_list ~f:Symex.simplify

      let rec pp fmt l =
        let open Format in
        match l with
        | [] -> fprintf fmt "[]"
        | [ v ] -> fprintf fmt "[%a]" Typed.ppa v
        | v :: vs ->
            fprintf fmt "[%a; " Typed.ppa v;
            pp fmt vs;
            fprintf fmt "]"

      let show l =
        l
        |> List.map (fun v ->
            v |> Typed.Expr.of_value |> Soteria.Tiny_values.Svalue.show)
        |> String.concat "; "

      let distinct _ = Typed.v_false
    end)

type invocation = {
  service : service;
  actual_args : symbolic_value env;
  ret_val : symbolic_value;
  successful : symb_bool;
  actual_qos : symbolic_value env;
}
(** Runtime structure representing a service invocation. *)

type history = invocation list
(** Represents the history of all service invocations made during the execution.
*)

type function_env = symbolic_value SymbolicListMap.t
(** Runtime structure representing a the current state of a function: the map
    encodes all the memoized function applications done so far using that
    function. *)

(** Possible causes of errors during the execution. *)
type error_cause =
  | DivByZeroError
  | PrecondError of service
  | PolicyError of int * policy_spec
  | AssertionError of bexpr

(** A map whose keys are error causes. It is used to aggregate errors after the
    execution and group them by cause. *)
module ErrorCauseMap = Map.Make (struct
  type t = error_cause located

  let compare_causes cause1 cause2 =
    match (cause1, cause2) with
    | DivByZeroError, DivByZeroError -> 0
    | PrecondError s1, PrecondError s2 -> String.compare s1.name s2.name
    | PolicyError (id1, _), PolicyError (id2, _) -> Int.compare id1 id2
    | AssertionError _, AssertionError _ -> 0
    | DivByZeroError, _ -> -1
    | _, DivByZeroError -> 1
    | PrecondError _, _ -> -1
    | _, PrecondError _ -> 1
    | PolicyError _, _ -> -1
    | _, PolicyError _ -> 1

  let compare cause1 cause2 =
    match compare_causes cause1.it cause2.it with
    | 0 -> compare cause1.at cause2.at
    | cmp -> cmp
end)

type fuel = { steps : Fuel.t; branching : Fuel.t; unroll : Fuel.t }
(** Runtime structure representing the current fuel available for execution. *)

type path_condition = symb_bool list
(** A path condition, which is a list of symbolic booleans (to be intended as in
    conjunction).*)

type ok_state = {
  scope : symbolic_value scope;
  history : history;
  fuel : fuel;
  function_envs : function_env env;
  service_map : service env;
}
(** Runtime structure representing the current state of the execution. *)

type err_state = {
  cause : error_cause located;
  err_scope : symbolic_value scope;
  err_history : history;
  function_envs : function_env env;
}
(** Runtime structure representing an erroneous execution. *)

(** Runtime structure representing a non-ok state, which can be either an
    unexplored branch or an error. It will be propagated till the end of the
    computation*)
type not_ok_state = Unexplored of ok_state | Err of err_state

type 'fix result = (ok_state, not_ok_state, 'fix) Compo_res.t * path_condition
(** A type alias for the result of a single branch execution. *)

let error_from_cause ~last_ok cause =
  Err
    {
      cause;
      err_scope = last_ok.scope;
      err_history = last_ok.history;
      function_envs = last_ok.function_envs;
    }
