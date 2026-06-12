open Format
open DataUtils

let rec pp_list_with_sep pp_value pp_sep fmt = function
  | [] -> fprintf fmt "<empty>"
  | [ x ] -> pp_value fmt x
  | x :: xs ->
      fprintf fmt "%a%a%a" pp_value x pp_sep ()
        (pp_list_with_sep pp_value pp_sep)
        xs

let rec pp_list pp fmt =
  pp_list_with_sep pp (fun fmt () -> fprintf fmt "@,") fmt

let rec pp_list_inline pp fmt =
  pp_list_with_sep pp (fun fmt () -> fprintf fmt ", ") fmt

let rec pp_env_entry pp_key pp_value fmt (key, value) =
  fprintf fmt "%a -> %a" pp_key key pp_value value

and pp_env pp_key pp_value fmt env =
  pp_list (pp_env_entry pp_key pp_value) fmt (StringMap.bindings env)

let pp_env_inline pp_key pp_value fmt env =
  pp_list_inline (pp_env_entry pp_key pp_value) fmt (StringMap.bindings env)
