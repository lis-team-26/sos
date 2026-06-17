open SymbolicData
open Soteria
open Expr.TypedAST
open Contract.TypedAST
open Utils.Data
open Utils.Types
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
module Compo_res = Soteria.Symex.Compo_res
include Symex.Syntax
module Fuel = Soteria.Symex.Fuel_gauge.Fuel_value

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

type 'a symbolic_list_env = 'a SymbolicListMap.t

type invocation = {
  service : service;
  actual_args : symbolic_value env;
  ret_val : symbolic_value;
  successful : symb_bool;
  actual_qos : symbolic_value env;
}

type stack = invocation list
type function_env = symbolic_value symbolic_list_env

type error_cause =
  | DivByZeroError
  | PrecondError of service
  | PolicyError of int * policy
  | AssertionError of bexpr

module ErrorCauseMap = Map.Make (struct
  type t = error_cause located

  let compare a b =
    match (a, b) with
    | { value = DivByZeroError }, { value = DivByZeroError } -> 0
    | { value = DivByZeroError }, _ -> -1
    | _, { value = DivByZeroError } -> 1
    | ( { value = PrecondError svc1; loc = loc1 },
        { value = PrecondError svc2; loc = loc2 } ) ->
        let c = compare loc1 loc2 in
        if c <> 0 then c else compare svc1.name svc2.name
    | { value = PrecondError _ }, _ -> -1
    | _, { value = PrecondError _ } -> 1
    | ( { value = PolicyError (idx1, _); loc = loc1 },
        { value = PolicyError (idx2, _); loc = loc2 } ) ->
        let c = compare loc1 loc2 in
        if c <> 0 then c else compare idx1 idx2
    | { value = PolicyError _ }, _ -> -1
    | _, { value = PolicyError _ } -> 1
    | ( { value = AssertionError _; loc = loc1 },
        { value = AssertionError _; loc = loc2 } ) ->
        compare loc1 loc2
end)

type fuel = { steps : Fuel.t; branching : Fuel.t; unroll : Fuel.t }
type path_condition = Typed.sbool list

type ok_state = {
  scope : symbolic_value scope;
  function_envs : function_env env;
  service_map : service env;
  ok_stack : stack;
  fuel : fuel;
}

type err_state = {
  err_stack : stack;
  function_envs : function_env env;
  cause : error_cause located;
}

type not_ok_state = Unexplored of ok_state | Err of err_state
type 'a result = (ok_state, not_ok_state, 'a) Symex.Result.t * path_condition
