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
                | Some (Error {id}) -> Some (id, pc))) results in
  List.fold_left (fun m (id, pCond) -> ViolMap.update id (function None -> Some [pCond] | Some l -> Some (pCond :: l)) m) ViolMap.empty xs

module IntSet = Set.Make(Int)
let get_vars expr =
  let vars = ref IntSet.empty
  in
  Typed.iter_vars expr (
      fun v -> vars := IntSet.add (v |> fst |> Soteria.Symex.Var.to_int) !vars
    ); !vars

let split_heuristic markedSet pathCondList =
  if List.for_all (List.for_all(fun exp -> (*ensure the split can take place*)
                       let vars = get_vars exp
                       in
                       (IntSet.subset vars markedSet) || (IntSet.disjoint vars markedSet)
       )) pathCondList
  then
    (*let disjNormalForm = List.map (List.filter (fun exp -> IntSet.subset (get_vars exp) markedSet))
    in (*TODO: check if UNSAT(not disjNormalForm). If it is SAT, then there exists an assignment to the
             variables marked as initial that makes the violation impossible, so it is not a manifest error.
             If it is UNSAT, then one of the path conditions must be SAT no matter the assignment to the marked variables*)*) false
  else false

(*Takes a set of variables marked as "initial" (markedSet) and the results of symbolic executions.
 Returns a list of bugs that may happen indipendently of what value is assigned to the variables in markedSet*)
let find_manif markedSet results =
  let groups = group_by_id results
  in
  groups
  |> ViolMap.to_seq
  |> Seq.filter_map (
         fun (viol, pathOr) ->
         if split_heuristic markedSet pathOr then
           Some viol
         else None
       )
  |> List.of_seq

