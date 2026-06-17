open Symbolic.Runtime
open Symbolic.Data
open Utils.Data

type error_result = {
  err_stack : stack;
  rev_function_map : IntSet.t IntMap.t;
  path_condition : symb_bool list;
}

let get_toplevel_vars expr =
  let vars = ref IntSet.empty in
  Typed.iter_vars expr (fun (x, _) ->
      vars := IntSet.add (Soteria.Symex.Var.to_int x) !vars);
  !vars

let get_toplevel_typed_vars expr =
  let vars = ref IntMap.empty in
  Typed.iter_vars expr (fun (x, t) ->
      vars := IntMap.add (Soteria.Symex.Var.to_int x) t !vars);
  !vars

let get_toplevel_vars_symbolic_value = function
  | SymbInt i -> get_toplevel_vars i
  | SymbBool b -> get_toplevel_vars b
  | SymbReceipt _ -> failwith "Unsupported type for manifest error heuristic"

let reverse_function_map function_envs =
  StringMap.fold
    (fun fname symb_map rev_map ->
      SymbolicListMap.syntactic_bindings symb_map
      |> Seq.fold_left
           (fun rev_map (args, return) ->
             let value = get_toplevel_vars_symbolic_value return in
             let argsSet =
               args |> List.map get_toplevel_vars
               |> List.fold_left IntSet.union IntSet.empty
             in
             if IntSet.cardinal value != 1 then
               failwith
                 "There should be a single nondet as the domain of the \
                  function map"
             else
               IntMap.update (IntSet.choose value)
                 (function
                   | None -> Some argsSet
                   | Some args -> Some (IntSet.union args argsSet))
                 rev_map)
           rev_map)
    function_envs IntMap.empty

let group_by_error_cause results =
  let error_results =
    List.filter_map
      (fun (state, path_condition) ->
        match Soteria.Symex.Compo_res.to_result_opt state with
        | None | Some (Ok _) | Some (Error (Unexplored _)) -> None
        | Some (Error (Err { err_stack; function_envs; cause })) ->
            Some
              ( cause,
                {
                  err_stack;
                  rev_function_map = reverse_function_map function_envs;
                  path_condition;
                } ))
      results
  in
  List.fold_left
    (fun error_cause_map (cause, error) ->
      ErrorCauseMap.update cause
        (function None -> Some [ error ] | Some l -> Some (error :: l))
        error_cause_map)
    ErrorCauseMap.empty error_results

let expand_vars variables reverse_function_map =
  let rec ev_rec vars =
    IntSet.fold
      (fun variable collected ->
        match IntMap.find_opt variable reverse_function_map with
        | None -> IntSet.add variable collected
        | Some arguments -> IntSet.union (ev_rec arguments) collected)
      vars IntSet.empty
  in
  ev_rec variables

let get_all_vars expr reverse_function_map =
  let vars = get_toplevel_vars expr in
  expand_vars vars reverse_function_map

let build_symex_assertion formula =
  let var_types = get_toplevel_typed_vars formula in
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
  let global_vals_set =
    Seq.ints 0 |> Seq.take (List.length global_vars) |> IntSet.of_seq
  in
  let splittable error =
    List.for_all
      (fun expr ->
        let vars = get_all_vars expr error.rev_function_map in
        IntSet.subset vars global_vals_set
        || IntSet.disjoint vars global_vals_set)
      error.path_condition
  in
  (* Ensure that the split can take place *)
  let error_list = List.filter splittable error_list in
  let formula =
    error_list
    |> List.map (fun error ->
        let depends_only_on_globals =
          (*set of symb values that depend only on globals*)
          error.rev_function_map |> IntMap.bindings |> List.map fst
          |> IntSet.of_list
          (*if all the leaves are globals, then the root depends only on globals*)
          |> IntSet.filter (fun var ->
              IntSet.subset
                (expand_vars (IntSet.singleton var) error.rev_function_map)
                global_vals_set)
          |> IntSet.union global_vals_set
          (*globals values also depend on globals, of course*)
        in
        List.filter
          (fun expr ->
            IntSet.subset (get_toplevel_vars expr) depends_only_on_globals)
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
