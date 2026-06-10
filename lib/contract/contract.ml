open ContractAST
open Utils.Parser
open Utils.Data
module AST = ContractAST
module AST_pp = ContractAST_pp
module TypedAST = TypedContractAST
module TypedAST_pp = TypedContractAST_pp
module Lexer = ContractLexer
module Parser = ContractParser

let parse src =
  let module Wrapper = MakeParser (struct
    type ast = AST.contract
    type token = Parser.token

    exception LexerError = Lexer.LexerError
    exception ParserError = Parser.Error

    let pp = AST_pp.pp_contract
    let lexer = Lexer.read
    let parser = Parser.contract
  end) in
  Wrapper.parse src

let type_check contract = TypeCheckContract.type_check_contract contract

(* TODO: enforce invariants for contracts: 
(v) no duplicate service names
(v) regex use services that are defined in the program
(v) policies only use services / QOS fields defined in the program
- (???) QoS constraints for each field of QoS vector
(v) type-check precondition (they can only mention globals, functions and parameters of the service)
- type check postcondtion:
  (v) qos_postcond:
    - effects: qos_field := expr (mentioning only globals, functions and parameters of the service)
    - constraints: expr (mentioning only qos fields, globals, functions and parameters of the service)
  (v) ok_postcond and err_postcond:
    - effects:
      - var (global or return variable) := expr (mentioning only globals, functions and parameters of the service)
      - function application ???
    - constraints: expr (mentioning only return variable, globals, functions and parameters of the service)
*)

let rec validate_regex (s2letter : AST.serv2letter)
    (service_names_set : StringSet.t) =
  List.for_all (fun (s, _) -> StringSet.mem s service_names_set) s2letter

let validate_contract (contract : AST.contract) =
  (* no duplicate service names *)
  let service_names =
    List.map (fun (s : AST.service) -> s.name) contract.services
  in
  let service_names_set = StringSet.of_list service_names in

  if List.length service_names <> StringSet.cardinal service_names_set then
    failwith "Duplicate service names found in the contract";

  (* no duplicate QoS fields *)
  let qos_fields = List.map fst contract.qos in
  let qos_fields_set = StringSet.of_list qos_fields in

  if List.length qos_fields <> StringSet.cardinal qos_fields_set then
    failwith "Duplicate QoS fields found in the contract";

  List.iter
    (fun (policy_type, _) ->
      match policy_type with
      (* bad prefix regex use services that are defined in the program*)
      | AST.Regex (s2letter, regex) ->
          if not (validate_regex s2letter service_names_set) then
            failwith "Regex in policy uses undefined service names"
      (* policies only use services / QOS fields defined in the program *)
      | AST.QosFieldOp (_, field, _, _) ->
          if not (List.mem field qos_fields) then
            failwith ("QoS policy uses undefined QoS field: " ^ field)
      | AST.Sort field ->
          if not (List.mem field qos_fields) then
            failwith ("Sort policy uses undefined QoS field: " ^ field))
    contract.policies;

  (* QoS constraints for each field of QoS vector *)
  (*
  - \forall service: \forall qos_field: \exists constraint
  *)
  (*
  List.iter
    (fun service ->
      let effct_vars =
        List.fold_left
          (fun acc (lhs, _) ->
            match lhs with
            | AST.LVar v -> StringSet.add v acc
            | AST.LApp (f, args) -> acc)
          StringSet.empty (fst service.qos_postcond)
      in
      let constrnt_vars =
        List.fold_left
          (fun acc arg -> StringSet.union acc (Expr.free_vars arg))
          StringSet.empty (snd service.qos_postcond)
      in
      let defined_qos_fields = StringSet.union effct_vars constrnt_vars in
      if not (StringSet.equal defined_qos_fields qos_fields_set) then
        let missing_fields = StringSet.diff qos_fields_set defined_qos_fields in
        let missing_fields_str =
          String.concat ", " (StringSet.elements missing_fields)
        in
        failwith
          ("Service " ^ service.name
         ^ " is missing QoS constraints for the field(s): " ^ missing_fields_str
          ))
    contract.services;
    *)
  let globals_vars = StringSet.of_list (List.map fst contract.globals) in
  let func_names = StringSet.of_list (List.map fst contract.functions) in
  let allowed_vars = StringSet.union globals_vars func_names in

  (*type-check precondition (they can only mention globals, functions and parameters of the service)*)
  let check_service_precond allowed_vars (service : AST.service) =
    let precond_vars =
      List.fold_left
        (fun acc e -> StringSet.union acc (Expr.free_vars e))
        StringSet.empty service.precond
    in
    let service_params = StringSet.of_list (List.map fst service.params) in
    let serv_allowed_vars = StringSet.union allowed_vars service_params in
    if not (StringSet.subset precond_vars serv_allowed_vars) then
      let disallowed_vars = StringSet.diff precond_vars serv_allowed_vars in
      let disallowed_vars_str =
        String.concat ", " (StringSet.elements disallowed_vars)
      in
      failwith
        ("Service " ^ service.name
       ^ " has precondition with undefined variables: " ^ disallowed_vars_str)
  in
  List.iter (check_service_precond allowed_vars) contract.services;

  (*- type check qos_postcond:
    - effects: qos_field := expr (mentioning only globals, functions and parameters of the service)
    - constraints: expr (mentioning only qos fields, globals, functions and parameters of the service)
    *)
  let qos_fields = StringSet.of_list (List.map fst contract.qos) in

  let check_service_qos_postcond qos_fields allowed_vars (service : AST.service)
      =
    let service_params = StringSet.of_list (List.map fst service.params) in
    let allowed_vars = StringSet.union allowed_vars service_params in

    let effects = fst service.qos_postcond in

    List.iter
      (fun (lhs, rhs) ->
        match lhs with
        | AST.LVar v ->
            if not (StringSet.mem v qos_fields) then
              failwith
                ("Service " ^ service.name
               ^ " has QoS postcondition effect assigning to undefined QoS \
                  field: " ^ v)
        | AST.LApp (f, args) ->
            if not (StringSet.mem f func_names) then
              failwith
                ("Service " ^ service.name
               ^ " has QoS postcondition effect applying undefined function: "
               ^ f);

            let args_vars =
              List.fold_left
                (fun acc arg -> StringSet.union acc (Expr.free_vars arg))
                StringSet.empty args
            in
            if not (StringSet.subset args_vars qos_fields) then
              failwith
                ("Service " ^ service.name
               ^ " has QoS postcondition effect applying function with \
                  undefined QoS field arguments");
            if not (StringSet.subset args_vars allowed_vars) then
              let diff_vars = StringSet.diff args_vars allowed_vars in
              let diff_vars_str =
                String.concat ", " (StringSet.elements diff_vars)
              in
              failwith
                ("Service " ^ service.name
               ^ " has QoS postcondition effect applying function" ^ f
               ^ " with undefined variable arguments: " ^ diff_vars_str)
            else ();

            let rhs_vars = Expr.free_vars rhs in
            if not (StringSet.subset rhs_vars allowed_vars) then
              let disallowed_vars = StringSet.diff rhs_vars allowed_vars in
              let disallowed_vars_str =
                String.concat ", " (StringSet.elements disallowed_vars)
              in
              failwith
                ("Service " ^ service.name
               ^ " has QoS postcondition effect with undefined variables: "
               ^ disallowed_vars_str))
      effects;

    let constraints = snd service.qos_postcond in
    List.iter
      (fun constr ->
        let constr_vars = Expr.free_vars constr in
        if not (StringSet.subset constr_vars qos_fields) then
          failwith
            ("Service " ^ service.name
           ^ " has QoS postcondition constraint with undefined QoS field \
              variables");
        let allowed_vars = StringSet.union allowed_vars qos_fields in
        if not (StringSet.subset constr_vars allowed_vars) then
          let disallowed_vars = StringSet.diff constr_vars allowed_vars in
          let disallowed_vars_str =
            String.concat ", " (StringSet.elements disallowed_vars)
          in
          failwith
            ("Service " ^ service.name
           ^ " has QoS postcondition constraint with undefined variables: "
           ^ disallowed_vars_str))
      constraints
  in
  List.iter
    (check_service_qos_postcond qos_fields allowed_vars)
    contract.services;

  (*type check ok_postcond and err_postcond:
    - effects:
      - var (global or return variable) := expr (mentioning only globals, functions and parameters of the service)
      - function application: params in (global U service param)
    - constraints: expr (mentioning only return variable, globals, functions and parameters of the service)
    *)
  let check_service_postcond allowed_vars (service : AST.service) =
    let service_params = StringSet.of_list (List.map fst service.params) in
    let allowed_vars = StringSet.union allowed_vars service_params in
    let return_var = StringSet.singleton (fst service.returns) in

    let effects =
      fst service.ok_postcond
      @ match service.err_postcond with None -> [] | Some post -> fst post
    in
    List.iter
      (fun (lhs, rhs) ->
        match lhs with
        | AST.LVar v ->
            if not (StringSet.mem v globals_vars || StringSet.mem v return_var)
            then
              failwith
                ("Service " ^ service.name
               ^ " has postcondition effect assigning to undefined variable: "
               ^ v)
        | AST.LApp (f, args) ->
            let args_vars =
              List.fold_left
                (fun acc arg -> StringSet.union acc (Expr.free_vars arg))
                StringSet.empty args
            in
            if not (StringSet.mem f func_names) then
              failwith
                ("Service " ^ service.name
               ^ " has postcondition effect applying undefined function: " ^ f);
            if not (StringSet.subset args_vars allowed_vars) then
              let diff_vars = StringSet.diff args_vars allowed_vars in
              let diff_vars_str =
                String.concat ", " (StringSet.elements diff_vars)
              in
              failwith
                ("Service " ^ service.name
               ^ " has postcondition effect applying function: " ^ f
               ^ " with undefined variable arguments: " ^ diff_vars_str
               ^ " while allowed variables are: "
                ^ String.concat ", " (StringSet.elements allowed_vars))
            else ();

            let rhs_vars = Expr.free_vars rhs in
            let allowed_vars = StringSet.union allowed_vars service_params in
            if not (StringSet.subset rhs_vars allowed_vars) then
              let disallowed_vars = StringSet.diff rhs_vars allowed_vars in
              let disallowed_vars_str =
                String.concat ", " (StringSet.elements disallowed_vars)
              in
              failwith
                ("Service " ^ service.name
               ^ " has postcondition effect with undefined variables: "
               ^ disallowed_vars_str))
      effects;
    let constraints =
      snd service.ok_postcond
      @ match service.err_postcond with None -> [] | Some post -> snd post
    in
    List.iter
      (fun constr ->
        let constr_vars = Expr.free_vars constr in
        let allowed_vars =
          StringSet.union allowed_vars
            (StringSet.union service_params return_var)
        in
        if not (StringSet.subset constr_vars allowed_vars) then
          let disallowed_vars = StringSet.diff constr_vars allowed_vars in
          let disallowed_vars_str =
            String.concat ", " (StringSet.elements disallowed_vars)
          in
          failwith
            ("Service " ^ service.name
           ^ " has postcondition constraint with undefined variables: "
           ^ disallowed_vars_str))
      constraints
  in
  List.iter (check_service_postcond allowed_vars) contract.services
