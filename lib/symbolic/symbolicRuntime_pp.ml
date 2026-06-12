open Format
open SymbolicRuntime
open SymbolicData_pp
open Utils.Data
open Utils.Data_pp
open Utils.Types_pp

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

let pp_invocation fmt { service; actual_args; actual_qos } =
  fprintf fmt "%s(%a), QoS: %a" service.name
    (pp_env_inline Fmt.string pp_value)
    actual_args
    (pp_env_inline Fmt.string pp_value)
    actual_qos

let rec pp_stack fmt stack = pp_list pp_invocation fmt stack

let rec pp_path_condition fmt pc =
  pp_list_with_sep Symex.Value.ppa (fun fmt () -> fprintf fmt " ∧@,") fmt pc

let pp_section fmt title is_empty pp_content content =
  if is_empty then fprintf fmt "%s: <empty>" title
  else fprintf fmt "%s:@,@[<v 2>  %a@]" title pp_content content

let pp_result fmt (idx, state, path_condition) =
  fprintf fmt "Result #%d:@,@[<v 2>  " idx;
  (match Compo_res.to_result_opt state with
  | Some (Ok { scope; ok_stack }) ->
      fprintf fmt "@{<green>SUCCESS@}@,";
      pp_section fmt "Path condition" (path_condition = []) pp_path_condition
        path_condition;
      fprintf fmt "@,";
      pp_section fmt "Scope stack" (scope = []) pp_scope scope;
      fprintf fmt "@,";
      pp_section fmt "Invocation stack" (ok_stack = []) pp_stack ok_stack
  | Some (Error { msg; err_stack }) ->
      fprintf fmt "@{<red>ERROR@}: %s@," msg;
      pp_section fmt "Path condition" (path_condition = []) pp_path_condition
        path_condition;
      fprintf fmt "@,";
      pp_section fmt "Invocation stack" (err_stack = []) pp_stack err_stack
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
