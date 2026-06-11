open Format
open SymbolicData
open Utils.Data

let rec pp_value fmt = function
  | SymbInt v -> Typed.ppa fmt v
  | SymbBool v -> Typed.ppa fmt v
  | SymbReceipt { ret_val; successful; qos_fields } ->
      fprintf fmt "{ retval = %a; successful = %a; qos = %a }" pp_value ret_val
        pp_value (SymbBool successful) pp_env qos_fields

and pp_env_entry fmt (key, value) = fprintf fmt "%s -> %a" key pp_value value

and pp_env fmt env =
  let entries = StringMap.bindings env in
  let rec pp_entries fmt = function
    | [] -> fprintf fmt "<empty>"
    | [ entry ] -> pp_env_entry fmt entry
    | entry :: rest -> fprintf fmt "%a@,%a" pp_env_entry entry pp_entries rest
  in
  pp_entries fmt entries

let pp_env_inline fmt env =
  match StringMap.bindings env with
  | [] -> ()
  | [ entry ] -> pp_env_entry fmt entry
  | entry :: rest ->
      pp_env_entry fmt entry;
      List.iter (fun item -> fprintf fmt ", %a" pp_env_entry item) rest
