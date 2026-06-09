module Data = DataUtils
module Parser = ParserUtils

module Types = struct
  type var_type = TInt | TBool
  type fun_type = TFun of var_type list * var_type
end
