open Symbolic.Runtime
open Symbolic.Data
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
  Typed.iter_vars expr (fun (x, _) ->
      vars := IntSet.add (Soteria.Symex.Var.to_int x) !vars);
  !vars

let build_symex_assertion global_vars formula =
  let* () =
    Symex.fold_list global_vars ~init:() ~f:(fun acc (x, t) ->
        match t with
        | Utils.Types.TInt ->
            let* v = Symex.nondet Typed.t_int in
            Symex.return ()
        | TBool ->
            let* v = Symex.nondet Typed.t_bool in
            Symex.return ()
        | _ ->
            failwith
              "Unreachable: unsupported type for manifest error heuristic")
  in
  (* Checks if UNSAT(not disj_normal_form). If it is SAT, then there exists an assignment to the
    variables marked as initial that makes the violation impossible, so it is not a manifest error.
    If it is UNSAT, then one of the path conditions must be SAT no matter the assignment to the marked variables. *)
  Symex.assert_ formula

let split_heuristic global_vars path_cond_list =
  let global_vals_set =
    Seq.ints 0 |> Seq.take (List.length global_vars) |> IntSet.of_seq
  in
  let splittable b =
    let vars = get_vars b in
    IntSet.subset vars global_vals_set || IntSet.disjoint vars global_vals_set
  in
  (* Ensure that the split can take place *)
  if List.for_all (List.for_all splittable) path_cond_list then
    let formula =
      path_cond_list
      |> List.map
           (List.filter (fun b -> IntSet.subset (get_vars b) global_vals_set))
      |> List.map (List.fold_left Typed.and_ Typed.v_true)
      |> List.fold_left Typed.or_ Typed.v_false
    in
    let results =
      Symex.run
        (build_symex_assertion global_vars formula)
        ~mode:Soteria.Symex.Approx.OX
      |> List.map fst
    in
    (* TODO: what should we return when the list of results is empty? *)
    List.for_all (fun x -> x) results
  else false

let find_manifest_errors global_vars results =
  let groups = group_by_error_cause results in
  groups |> ErrorCauseMap.bindings
  |> List.filter_map (fun (cause, path_cond_list) ->
      if split_heuristic global_vars path_cond_list then Some cause else None)
