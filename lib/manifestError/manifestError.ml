open Symbolic.Runtime
open Symbolic.Data
open Utils.Data

type error_result = {
  err_stack : stack;
  initial_returns : IntSet.t;
  path_condition : symb_bool list;
}

(* After creating a symbolic value with nondet, get the id of the value*)
let get_var_as_int sv =
  match Typed.kind sv with
  | Var v -> Soteria.Symex.Var.to_int v
  | _ -> failwith "this wasn't a nondet, it was another expression"

let get_vars expr =
  let vars = ref IntSet.empty in
  Typed.iter_vars expr (fun (x, _) ->
      vars := IntSet.add (Soteria.Symex.Var.to_int x) !vars);
  !vars

let get_typed_vars expr =
  let vars = ref IntMap.empty in
  Typed.iter_vars expr (fun (x, t) ->
      vars := IntMap.add (Soteria.Symex.Var.to_int x) t !vars);
  !vars

let group_by_error_cause results =
  let error_results =
    List.filter_map
      (fun (state, path_condition) ->
        match Soteria.Symex.Compo_res.to_result_opt state with
        | None | Some (Ok _) | Some (Error (Unexplored _)) -> None
        | Some (Error (Err { err_stack; initial_returns; cause })) ->
            Some (cause, { err_stack; initial_returns; path_condition }))
      results
  in
  List.fold_left
    (fun error_cause_map (cause, error) ->
      ErrorCauseMap.update cause
        (function None -> Some [ error ] | Some l -> Some (error :: l))
        error_cause_map)
    ErrorCauseMap.empty error_results

let build_symex_assertion formula =
  let var_types = get_typed_vars formula in
  let var_idx =
    Seq.ints 0
    |> Seq.take
         (match IntMap.max_binding_opt var_types with
         | None -> (*no variables in formula*) 0
         | Some (maximum, _) -> maximum)
    |> List.of_seq
  in
  let* () =
    Symex.fold_list var_idx ~init:() ~f:(fun acc i ->
        match IntMap.find_opt i var_types with
        | Some t ->
            if Typed.is_bool_ty t then
              let* v = Symex.nondet Typed.t_bool in
              Symex.return ()
            else
              let* v = Symex.nondet Typed.t_int in
              Symex.return ()
        | None ->
            let* v = Symex.nondet Typed.t_int in
            Symex.return ())
  in
  (* Checks if UNSAT(not disj_normal_form). If it is SAT, then there exists an assignment to the
    variables marked as initial that makes the violation impossible, so it is not a manifest error.
    If it is UNSAT, then one of the path conditions must be SAT no matter the assignment to the marked variables. *)
  Symex.assert_ formula

let split_heuristic global_vars error_list =
  let global_vals =
    Seq.ints 0 |> Seq.take (List.length global_vars) |> IntSet.of_seq
  in
  let splittable error =
    List.for_all
      (fun expr ->
        let initial = IntSet.union global_vals error.initial_returns in
        let vars = get_vars expr in
        IntSet.subset vars initial || IntSet.disjoint vars initial)
      error.path_condition
  in
  (* Ensure that the split can take place *)
  let error_list = List.filter splittable error_list in
  let formula =
    error_list
    |> List.map (fun error ->
        List.filter
          (fun expr ->
            IntSet.subset (get_vars expr)
              (IntSet.union global_vals error.initial_returns))
          error.path_condition)
    |> List.map (List.fold_left Typed.and_ Typed.v_true)
    |> List.fold_left Typed.or_ Typed.v_false
  in
  let results =
    Symex.run (build_symex_assertion formula) ~mode:Soteria.Symex.Approx.OX
    |> List.map fst
  in
  (* TODO: what should we return when the list of results is empty? *)
  List.for_all (fun x -> x) results

let find_manifest_errors global_vars results =
  let groups = group_by_error_cause results in
  groups |> ErrorCauseMap.bindings
  |> List.filter_map (fun (cause, error_list) ->
      if split_heuristic global_vars error_list then Some cause else None)
