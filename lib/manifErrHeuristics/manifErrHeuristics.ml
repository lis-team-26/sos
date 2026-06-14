open Symbolic.Runtime
open Symbolic.Data
open Soteria.Tiny_values.Tiny_solver.Z3_solver
open Utils.Data

let group_by_error_cause results =
  let error_results =
    List.filter_map
      (fun (state, path_cond) ->
        match Soteria.Symex.Compo_res.to_result_opt state with
        | None | Some (Ok _) -> None
        | Some (Error { cause }) -> Some (cause, path_cond))
      results
  in
  List.fold_left
    (fun error_cause_map (cause, path_cond) ->
      ErrorCauseMap.update cause
        (function
          | None -> Some [ path_cond ] | Some l -> Some (path_cond :: l))
        error_cause_map)
    ErrorCauseMap.empty error_results

let get_vars expr =
  let vars = ref IntSet.empty in
  Typed.iter_vars expr (fun (id, _) ->
      vars := IntSet.add (id |> Soteria.Symex.Var.to_int) !vars);
  !vars

let split_heuristic markedSet pathCondList =
  if
    List.for_all
      (List.for_all (fun exp ->
           (*ensure the split can take place*)
           let vars = get_vars exp in
           IntSet.subset vars markedSet || IntSet.disjoint vars markedSet))
      pathCondList
  then
    let disjNormalForm =
      List.map
        (List.filter (fun exp -> IntSet.subset (get_vars exp) markedSet))
        pathCondList
      (*check if UNSAT(not disjNormalForm). If it is SAT, then there exists an assignment to the
    variables marked as initial that makes the violation impossible, so it is not a manifest error.
    If it is UNSAT, then one of the path conditions must be SAT no matter the assignment to the marked variables*)
    in
    let formula =
      List.fold_left
        (fun f pc -> Typed.or_ f (Typed.conj pc))
        Typed.v_false disjNormalForm
    in
    let negFormula = Typed.not formula in
    let solv = init () in
    let () = add_constraints solv [ negFormula ] in
    Soteria.Symex.Solver_result.is_unsat (sat solv)
  else false

(** Takes a list of variables marked as "initial" [marked] and the results of
    symbolic executions. Returns a list of bugs that may happen indipendently of
    what value is assigned to the variables in [marked]*)
let find_manifest_errors marked results =
  let marked_set = IntSet.of_list marked in
  let groups = group_by_error_cause results in
  groups |> ErrorCauseMap.bindings
  |> List.filter_map (fun (cause, path_cond_list) ->
      if split_heuristic marked_set path_cond_list then Some cause else None)
