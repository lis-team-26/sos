open Format
open SymbolicData
open Utils.Data

let pp_value fmt = function
  | SymbInt v -> Typed.ppa fmt v
  | SymbBool v -> Typed.ppa fmt v

let pp_env_entry fmt (key, value) = fprintf fmt "%s -> %a" key pp_value value

let pp_env_inline fmt env =
  match StringMap.bindings env with
  | [] -> ()
  | [ entry ] -> pp_env_entry fmt entry
  | entry :: rest ->
      pp_env_entry fmt entry;
      List.iter (fun item -> fprintf fmt ", %a" pp_env_entry item) rest

let pp_env fmt env =
  let entries = StringMap.bindings env in
  match entries with
  | [] -> fprintf fmt "{}"
  | _ ->
      fprintf fmt "{\n";
      List.iter
        (fun (key, value) -> fprintf fmt "  %s -> %a\n" key pp_value value)
        entries;
      fprintf fmt "}"
