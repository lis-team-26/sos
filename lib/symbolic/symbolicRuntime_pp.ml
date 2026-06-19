open Format
open SymbolicRuntime
open SymbolicData
open SymbolicData_pp
open Utils.Data
open Utils.Data_pp
open Utils.Types_pp
open Expr.TypedAST_pp
open Contract.TypedAST_pp

let setup_color_tags fmt =
  let red = "\027[31m" in
  let green = "\027[32m" in
  let yellow = "\027[33m" in
  let reset = "\027[0m" in
  let mark_open_tag = function
    | "red" -> red
    | "green" -> green
    | "yellow" -> yellow
    | _ -> ""
  in
  let mark_close_tag _ = reset in
  pp_set_formatter_stag_functions fmt
    {
      (pp_get_formatter_stag_functions fmt ()) with
      mark_open_stag = (function String_tag s -> mark_open_tag s | _ -> "");
      mark_close_stag = (fun _ -> mark_close_tag ());
    };
  pp_set_mark_tags fmt true

let pp_scope fmt scope =
  let scope_with_idxs = List.mapi (fun idx env -> (idx + 1, env)) scope in
  let public_scope_idx = List.length scope in
  pp_list
    (fun fmt (idx, env) ->
      let env_name =
        if idx = public_scope_idx then "Public Environment"
        else Fmt.str "Environment #%d" idx
      in
      fprintf fmt "%s: @,@[<v 2>  %a@]" env_name
        (pp_env Fmt.string pp_value)
        env)
    fmt scope_with_idxs

let pp_invocation fmt (idx, invocation) =
  fprintf fmt
    "(#%d) %s@,\
     @[<v 2>  Args: { %a }@,\
     Returned: %a@,\
     Successful: %a@,\
     QoS: { %a }@]"
    idx invocation.service.name
    (pp_env_inline Fmt.string pp_value)
    invocation.actual_args pp_value invocation.ret_val Symex.Value.ppa
    invocation.successful
    (pp_env_inline Fmt.string pp_value)
    invocation.actual_qos

let rec pp_stack fmt stack = pp_list pp_invocation fmt stack

let rec pp_path_condition fmt pc =
  pp_list_with_sep Symex.Value.ppa (fun fmt () -> fprintf fmt " ∧@,") fmt pc

let pp_function_env fmt function_env =
  fprintf fmt "  @,@[<v 2>  ";
  pp_list
    (fun fmt (key_args, value) ->
      fprintf fmt "[ %a ] -> %a"
        (pp_list_inline Symex.Value.ppa)
        key_args pp_value value)
    fmt
    (function_env |> SymbolicListMap.syntactic_bindings |> List.of_seq);
  fprintf fmt "@]"

let pp_function_envs fmt function_envs =
  pp_env Fmt.string pp_function_env fmt function_envs

let pp_section fmt title is_empty pp_content content =
  if is_empty then fprintf fmt "%s: <empty>" title
  else fprintf fmt "%s:@,@[<v 2>  %a@]" title pp_content content

let pp_located_error_cause fmt = function
  | { value = DivByZeroError } -> fprintf fmt "Division by zero"
  | { value = PrecondError svc; loc = Loc { line; col } } ->
      fprintf fmt "%s service precondition not satisfied at line %d, column %d"
        svc.name line col
  | { value = PolicyError (idx, policy); loc = Loc { line; col } } ->
      fprintf fmt "Policy violation at line %d, column %d: %a" line col
        pp_policy policy
  | { value = PolicyError (idx, policy); loc = NoLoc } ->
      fprintf fmt "Policy violation at index %d: %a" idx pp_policy policy
  | { value = AssertionError bexpr; loc = Loc { line; col } } ->
      fprintf fmt "Assertion failed at line %d, column %d: %a" line col pp_bexpr
        bexpr
  | _ -> failwith "Unreachable: Unexpected error cause or location"

let pp_result fmt (idx, state, path_condition) =
  fprintf fmt "Result #%d:@,@[<v 2>  " idx;
  (match Compo_res.to_result_opt state with
  | Some (Ok { scope; function_envs; ok_stack }) ->
      fprintf fmt "@{<green>SUCCESS@}@,";
      pp_section fmt "Path condition" (path_condition = []) pp_path_condition
        path_condition;
      fprintf fmt "@,";
      pp_section fmt "Scope stack" (scope = []) pp_scope scope;
      fprintf fmt "@,";
      pp_section fmt "Functions environment"
        (StringMap.is_empty function_envs)
        pp_function_envs function_envs;
      fprintf fmt "@,";
      pp_section fmt "Invocation stack" (ok_stack = []) pp_stack
        (List.mapi (fun idx inv -> (idx + 1, inv)) ok_stack)
  | Some (Error (Err { cause; function_envs; err_stack })) ->
      fprintf fmt "@{<red>ERROR@}: %a@," pp_located_error_cause cause;
      pp_section fmt "Path condition" (path_condition = []) pp_path_condition
        path_condition;
      fprintf fmt "@,";
      pp_section fmt "Invocation stack" (err_stack = []) pp_stack
        (List.mapi (fun idx inv -> (idx + 1, inv)) err_stack)
  | Some (Error (Unexplored { scope; function_envs; ok_stack })) ->
      fprintf fmt "@{<yellow>UNEXPLORED@}@,";
      pp_section fmt "Scope stack" (scope = []) pp_scope scope;
      fprintf fmt "@,";
      pp_section fmt "Functions environment"
        (StringMap.is_empty function_envs)
        pp_function_envs function_envs;
      fprintf fmt "@,";
      pp_section fmt "Invocation stack" (ok_stack = []) pp_stack
        (List.mapi (fun idx inv -> (idx + 1, inv)) ok_stack);
      fprintf fmt "@,";
      pp_section fmt "Path condition" (path_condition = []) pp_path_condition
        path_condition
  | None ->
      pp_section fmt "Path condition" (path_condition = []) pp_path_condition
        path_condition;
      fprintf fmt "@,";
      fprintf fmt "Result: Unknown");
  fprintf fmt "@]"

let pp_results fmt results =
  let results_with_idxs =
    List.mapi (fun idx (s, pc) -> (idx + 1, s, pc)) results
  in
  setup_color_tags fmt;
  fprintf fmt "@[<v 0>%a@,@]" (pp_list pp_result) results_with_idxs

let pp_manifest_errors fmt vids =
  fprintf fmt "@[<v 0>%a@,@]" (pp_list pp_located_error_cause) vids
