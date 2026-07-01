open Utils.Loc

val type_check_contract :
  ContractAST.contract -> (TypedContractAST.contract, string located) result
(** Type-checks a contract specification, by ensuring that:
    - the service names are unique;
    - the global variable names are unique;
    - the QoS field names are unique;
    - the function names are unique;
    - the global assumptions are boolean expressions mentioning only the global
      variables and not containing function applications;
    - each service is well-formed;
    - each policy is well-formed. *)
