open Format
open SymbolicData
open Utils.Data
open Utils.Data_pp

let rec pp_value fmt = function
  | SymbInt v -> Typed.ppa fmt v
  | SymbBool v -> Typed.ppa fmt v
  | SymbReceipt { ret_val; successful; qos_fields } ->
      fprintf fmt "{ retval = %a; successful = %a; qos = %a }" pp_value ret_val
        pp_value (SymbBool successful)
        (pp_env Fmt.string pp_value)
        qos_fields
