open Format
open SymbolicRuntime
open SymbolicData_pp
open Utils.Data

let red = "\027[31m"
let green = "\027[32m"
let yellow = "\027[33m"
let reset = "\027[0m"

let setup_color_tags fmt =
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

let rec pp_list pp fmt = function
  | [] -> ()
  | [ x ] -> pp fmt x
  | x :: xs -> fprintf fmt "%a@,%a" pp x (pp_list pp) xs

let pp_invocation fmt { service; actual_args; actual_qos } =
  fprintf fmt "%s(%a), QoS: %a" service.name pp_env_inline actual_args
    pp_env_inline actual_qos

let rec pp_stack fmt stack =
  match stack with
  | [] -> fprintf fmt "<empty>"
  | [ invocation ] -> fprintf fmt "%a" pp_invocation invocation
  | invocation :: rest ->
      fprintf fmt "%a@,%a" pp_invocation invocation pp_stack rest

let rec pp_path_condition fmt = function
  | [] -> fprintf fmt "<empty>"
  | [ b ] -> Symex.Value.ppa fmt b
  | b :: bs -> fprintf fmt "%a ∧@,%a" Symex.Value.ppa b pp_path_condition bs

let pp_section fmt title is_empty pp_content content =
  if is_empty then fprintf fmt "%s: <empty>" title
  else fprintf fmt "%s:@,@[<v 2>  %a@]" title pp_content content

let pp_result fmt (idx, state, path_condition) =
  fprintf fmt "Result #%d:@,@[<v 2>  " idx;
  (match Compo_res.to_result_opt state with
  | Some (Ok { scope; ok_stack }) ->
      let public_env = get_public_env scope in
      fprintf fmt "@{<green>SUCCESS@}@,";
      pp_section fmt "Path condition" (path_condition = []) pp_path_condition
        path_condition;
      fprintf fmt "@,";
      pp_section fmt "Public environment"
        (StringMap.is_empty public_env)
        pp_env public_env;
      fprintf fmt "@,";
      pp_section fmt "Invocation stack" (ok_stack = []) pp_stack ok_stack;
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
  let xs = List.mapi (fun i (s, pc) -> (i + 1, s, pc)) results in
  setup_color_tags fmt;
  fprintf fmt "@[<v 0>%a@,@]" (pp_list pp_result) xs
