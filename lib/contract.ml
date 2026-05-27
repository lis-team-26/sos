module AST = ContractAST
module AST_pp = ContractAST_pp
module Lexer = ContractLexer
module Parser = ContractParser

let parse src =
  let module Wrapper = Utils.MakeParser (struct
    type ast = AST.contract
    type token = Parser.token

    exception LexerError = Lexer.LexerError
    exception ParserError = Parser.Error

    let pp = AST_pp.pp_contract
    let lexer = Lexer.read
    let parser = Parser.contract
  end) in
  Wrapper.parse src


(* TODO: enforce invariants for contracts: 
(v) no duplicate service names
(v) regex use services that are defined in the program
(v) policies only use services / QOS fields defined in the program
- QoS constraints for each field of QoS vector
*)

module StringSet = Set.Make(String)

let rec validate_regex (s2letter: AST.serv2letter) (service_names_set: StringSet.t) =
  List.for_all (fun (s,_) -> StringSet.mem s service_names_set) s2letter

let validate_contract (contract: AST.contract) =
  
  (* no duplicate service names *)
  let service_names = List.map (fun (s: AST.service) -> s.name) contract.services in
  let service_names_set = StringSet.of_list service_names in
  
  if List.length service_names <> StringSet.cardinal service_names_set then
    failwith "Duplicate service names found in the contract";

  (* no duplicate QoS fields *)
  let qos_fields = List.map fst contract.qos in
  let qos_fields_set = StringSet.of_list qos_fields in

  if List.length qos_fields <> StringSet.cardinal qos_fields_set then
    failwith "Duplicate QoS fields found in the contract";

  List.iter (fun (policy_type, _) ->
    match policy_type with
    (* bad prefix regex use services that are defined in the program*)
    | AST.Regex (s2letter, regex) -> 
        if not (validate_regex s2letter service_names_set) then
          failwith "Regex in policy uses undefined service names"
    (* policies only use services / QOS fields defined in the program *)      
    | AST.QosFieldOp (_, _, field, _) ->
        let qos_fields = "trust" :: (List.map fst contract.qos) in
        if not (List.mem field qos_fields) then
          failwith ("QoS policy uses undefined QoS field: " ^ field) 
    | AST.Sort _ -> ()
  ) contract.policies;

  (* QoS constraints for each field of QoS vector *)
  (*
  - \forall service: \forall qos_field: \exists constraint
  *)
  List.iter (fun (service: AST.service) ->
    let effect_vars = List.fold_left (fun acc (lhs, _) ->
      match lhs with
      | AST.LVar v -> StringSet.add v acc
      | AST.LApp (f, args) ->
          List.fold_left (fun acc arg ->
            match arg with
            | AST.EVar v -> StringSet.add v acc
            | _ -> acc
          ) acc args
    ) StringSet.empty (fst service.qos) in
    let constrnt_fields = List.fold_left (fun acc (binop, lhs, expr) ->
      match lhs with
      | AST.LVar v -> StringSet.add v acc
      | AST.LApp (f, args) ->
          List.fold_left (fun acc arg ->
            match arg with
            | AST.EVar v -> StringSet.add v acc
            | _ -> acc
          ) acc args
    ) StringSet.empty (snd service.qos) in
    let qos_constraints = StringSet.union effect_vars constrnt_fields in
    if not (StringSet.equal qos_constraints qos_fields_set) then
      let missing_fields = StringSet.diff qos_fields_set qos_constraints in
      let missing_fields_str = String.concat ", " (StringSet.elements missing_fields) in
      failwith ("Service " ^ service.name ^ " is missing QoS constraints:for the field(s): " ^ missing_fields_str)
  ) contract.services

