open SymbolicData
open Soteria
open Expr.AST
open Contract.AST
open Utils.Data
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
module Compo_res = Soteria.Symex.Compo_res
include Symex.Syntax

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
  failed : symb_bool;
  qos : symbolic_value env;
}

type stack = invocation list
type function_map = (fun_type * symbolic_value symbolic_list_env) env

type ok_state = {
  private_env : symbolic_value env;
  public_env : symbolic_value env;
  service_map : service env;
  function_map : function_map;
  stack : stack;
}

type err_state = { msg : string; stack : stack }
type path_condition = Typed.sbool list
type 'a result = (ok_state, err_state, 'a) Symex.Result.t * path_condition
