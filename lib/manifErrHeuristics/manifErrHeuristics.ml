open Symbolic.Runtime
open Symbolic.Data
open Utils.Data

type violation_id =
  | ServicePrecond of string
  | Policy of int
  | Assert of int

(*replace actual err_state with this in symbolic.runtime once finished*)
type err_state = { msg : string; err_stack : stack; id : violation_id }
type 'a result = (ok_state, err_state, 'a) Symex.Result.t * path_condition

module Violation = struct
    type t = violation_id
    let compare a b = match (a,b) with
      | (ServicePrecond sa, ServicePrecond sb) -> String.compare sa sb
      | (Policy pa, Policy pb) -> Int.compare pa pb
      | (Assert aa, Assert ab) -> Int.compare aa ab
      | (ServicePrecond _, Policy _) -> -1
      | (ServicePrecond _, Assert _) -> -1
      | (Policy _, ServicePrecond _) -> 1
      | (Assert _, ServicePrecond _) -> 1
      | (Policy _, Assert _) -> -1
      | (Assert _, Policy _) -> 1
end;;

module ViolMap = Map.Make(Violation)
let group_by_id results =
  let xs = List.filter_map (fun (s, pc) ->
               (match Compo_res.to_result_opt s with
                | None | Some (Ok _) -> None
                | Some (Error {id}) -> Some (id, List.map Symex.Value.Expr.of_value pc))) results in
  List.fold_left (fun m (id, pCond) -> ViolMap.update id (function None -> Some [pCond] | Some l -> Some (pCond :: l)) m) ViolMap.empty xs
let absence_heuristic markedSet pathCondList = 0
let split_heuristic markedSet pathCondList= 0
