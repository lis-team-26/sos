open Format
open TypesUtils
open DataUtils_pp

let rec pp_var_type fmt = function
  | TInt -> fprintf fmt "int"
  | TBool -> fprintf fmt "bool"
  | TReceipt { ret_type } ->
      fprintf fmt "rcpt { retval: %a }" pp_var_type ret_type

let rec pp_var_type_list fmt =
  pp_list_with_sep pp_var_type (fun fmt () -> fprintf fmt " -> ") fmt

and pp_fun_type fmt = function
  | TFun (ts, t) -> fprintf fmt "%a -> %a" pp_var_type_list ts pp_var_type t

let pp_typed_var fmt (x, t) = fprintf fmt "%s: %a" x pp_var_type t
let pp_typed_fun fmt (f, t) = fprintf fmt "%s: %a" f pp_fun_type t
