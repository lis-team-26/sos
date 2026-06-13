open Symbolic.Runtime
open Symbolic.Data

let viol_id_hash = function
  | DivByZero | ServicePrecond _ -> (0, 0)
  | Policy n -> (1, n)
  | AssertFail line -> (2, line)

module Violation = struct
  type t = violation_id

  let compare a b =
    match (a, b) with
    | ServicePrecond sa, ServicePrecond sb -> String.compare sa sb
    | ServicePrecond _, _ -> -1
    | _, ServicePrecond _ -> 1
    | a, b ->
        let a1, a2 = viol_id_hash a in
        let b1, b2 = viol_id_hash b in
        if a1 == b1 then Int.compare a2 b2 else Int.compare a1 b1
end

module ViolMap = Map.Make (Violation)

let group_by_id results =
  let xs =
    List.filter_map
      (fun (s, pc) ->
        match Compo_res.to_result_opt s with
        | None | Some (Ok _) -> None
        | Some (Error { vid }) -> Some (vid, pc))
      results
  in
  List.fold_left
    (fun m (id, pCond) ->
      ViolMap.update id
        (function None -> Some [ pCond ] | Some l -> Some (pCond :: l))
        m)
    ViolMap.empty xs

module IntSet = Set.Make (Int)

let get_vars expr =
  let vars = ref IntSet.empty in
  Typed.iter_vars expr (fun v ->
      vars := IntSet.add (v |> fst |> Soteria.Symex.Var.to_int) !vars);
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
    let solv = Soteria.Tiny_values.Tiny_solver.Z3_solver.init () in
    let () =
      Soteria.Tiny_values.Tiny_solver.Z3_solver.add_constraints solv
        [ negFormula ]
    in
    Soteria.Symex.Solver_result.is_unsat
      (Soteria.Tiny_values.Tiny_solver.Z3_solver.sat solv)
  else false

(*Takes a set of variables marked as "initial" (markedSet) and the results of symbolic executions.
 Returns a list of bugs that may happen indipendently of what value is assigned to the variables in markedSet*)
let find_manif marked results =
  let markedSet = IntSet.of_list marked
  in
  let groups = group_by_id results in
  groups |> ViolMap.to_seq
  |> Seq.filter_map (fun (viol, pathOr) ->
      if split_heuristic markedSet pathOr then Some viol else None)
  |> List.of_seq
