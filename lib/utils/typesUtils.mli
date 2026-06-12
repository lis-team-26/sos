type var_type =
  | TInt
  | TBool
  | TReceipt of { ret_type : var_type; qos_types : var_type DataUtils.StringMap.t }

type fun_type = TFun of var_type list * var_type
