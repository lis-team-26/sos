open SymbolicData
open Soteria
open Expr.TypedAST
open Contract.TypedAST
open Utils.Data
open Utils.Types
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
module Compo_res = Soteria.Symex.Compo_res
include Symex.Syntax

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

type ok_state = {
  scope : symbolic_value scope;
  function_envs : function_env env;
  service_map : service env;
  ok_stack : stack;
}

type error_cause =
  | DivByZero
  | ServicePrecond of string
  | Policy of int * policy
  | AssertFail of int * bexpr

module ErrorCauseMap = Map.Make (struct
  type t = error_cause

  let viol_id_hash = function
    | DivByZero | ServicePrecond _ -> (0, 0)
    | Policy (n, _) -> (1, n)
    | AssertFail (line, _) -> (2, line)

  let compare a b =
    match (a, b) with
    | ServicePrecond sa, ServicePrecond sb -> String.compare sa sb
    | ServicePrecond _, _ -> -1
    | _, ServicePrecond _ -> 1
    | a, b ->
        let a1, a2 = viol_id_hash a in
        let b1, b2 = viol_id_hash b in
        if a1 == b1 then Int.compare a2 b2 else Int.compare a1 b1
end)

type err_state = { err_stack : stack; cause : error_cause }
type path_condition = Typed.sbool list
type 'a result = (ok_state, err_state, 'a) Symex.Result.t * path_condition
