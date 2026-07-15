open Symbolic.Runtime
open Symbolic.Data
open Utils.Loc
open Utils.Data
open Expr.TypedAST
open Contract.AST
open Contract.TypedAST
open Reg2dfa
open PolicyCheckerData

let raise_violation ~loc policy =
  Symex.Result.error (located ~loc (PolicyError (policy.id, policy.spec)))

let cmp_fun_of_cmp_op = function
  | Lt -> Typed.lt
  | Le -> Typed.leq
  | Gt -> Typed.gt
  | Ge -> Typed.geq
  | Eq -> Typed.sem_eq
  | Neq -> fun x y -> Typed.not @@ Typed.sem_eq x y

let aggr_fun_of_aggr_op = function
  | Sum -> fun x y -> Typed.add x y
  | Max -> fun x y -> Typed.ite (Typed.gt y x) y x
  | Min -> fun x y -> Typed.ite (Typed.lt y x) y x
  | Avg -> failwith "Unreachable: average policies should be handled separately"

(** Helper function, which applies a check function [f] to every group's
    accumulated state. For [Ungrouped], applies [f] once to the single
    accumulated state. For [Grouped], it uses [SymbolicMap.find_opt] with a
    fresh symbolic key to let Soteria branch over all possible groups under
    their respective path conditions. *)
let check_each_group f = function
  | Ungrouped s -> f s
  | Grouped (_, symb_map) -> (
      (* [syntactic_bindings] returns the list of (key, value) pairs that have
         been inserted with [syntactic_add] — i.e. the concrete groups built
         during [update_policy]. We fold over them sequentially and stop at the
         first violation (the monadic bind propagates errors). *)
      let bindings = List.of_seq (SymbolicMap.syntactic_bindings symb_map) in
      match bindings with
      | [] ->
          (* No invocations were ever grouped: nothing to verify *)
          Symex.Result.ok ()
      | _ ->
          List.fold_left
            (fun acc (_, state) ->
              let** () = acc in
              f state)
            (Symex.Result.ok ()) bindings)
