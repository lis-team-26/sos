(** Static variable types. In [TReceipt] we need to store its return type and
    its QoS types to ensure type safety with receipt variables (the successful
    field is not included as it is always a boolean). *)
type var_type =
  | TInt
  | TBool
  | TReceipt of {
      ret_type : var_type;
      qos_types : var_type DataUtils.StringMap.t;
    }

(** Function types. *)
type fun_type = TFun of var_type list * var_type
