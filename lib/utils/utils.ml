module Data = DataUtils
module Parser = ParserUtils

module Types = struct
  type var_type =
    | TInt
    | TBool
    | TReceipt of { ret_type : var_type; qos_types : var_type Data.StringMap.t }

  type fun_type = TFun of var_type list * var_type
end
