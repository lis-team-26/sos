open Format
open TypesUtils
open DataUtils_pp

let rec pp_var_type fmt = function
  | TInt -> Fmt.pf fmt "int"
  | TBool -> Fmt.pf fmt "bool"
  | TReceipt { ret_type } ->
      Fmt.pf fmt "rcpt { retval: %a }" pp_var_type ret_type

and pp_fun_type fmt = function
  | TFun (ts, t) ->
      Fmt.pf fmt "%a -> %a"
        (pp_list ~sep:(Fmt.const Fmt.string " -> ") pp_var_type)
        ts pp_var_type t

let pp_typed_var fmt (x, t) = Fmt.pf fmt "%s: %a" x pp_var_type t
let pp_typed_fun fmt (f, t) = Fmt.pf fmt "%s: %a" f pp_fun_type t
