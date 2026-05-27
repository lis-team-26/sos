open ContractAST
open Utils.Parser
open Utils.Data
module AST = ContractAST
module AST_pp = ContractAST_pp
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

(* TODO: enforce invariants for contracts: 
(v) no duplicate service names
(v) regex use services that are defined in the program
(v) policies only use services / QOS fields defined in the program
- (???) QoS constraints for each field of QoS vector
- type-check precondition (they can only mention globals, functions and parameters of the service)
- type check postcondtion:
  - qos_postcond:
    - effects: qos_field := expr (mentioning only globals, functions and parameters of the service)
    - constraints: expr (mentioning only qos fields, globals, functions and parameters of the service)
  - ok_postcond and err_postcond:
    - effects:
      - var (global or return variable) := expr (mentioning only globals, functions and parameters of the service)
      - function application ???
    - constraints: expr (mentioning only return variable, globals, functions and parameters of the service)
*)

let rec validate_regex (regex : AST.regex) (service_names_set : StringSet.t) =
  match regex with
  | AST.RService s -> StringSet.mem s service_names_set
  | AST.RConcat (r1, r2) | AST.RChoice (r1, r2) ->
      validate_regex r1 service_names_set && validate_regex r2 service_names_set
  | AST.RStar r -> validate_regex r service_names_set

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
      | AST.Regex regex ->
          if not (validate_regex regex service_names_set) then
            failwith "Regex in policy uses undefined service names"
      (* policies only use services / QOS fields defined in the program *)
      | AST.QosFieldOp (_, field, _, _) ->
          let qos_fields = "trust" :: List.map fst contract.qos in
          if not (List.mem field qos_fields) then
            failwith ("QoS policy uses undefined QoS field: " ^ field)
      | AST.Sort _ -> ())
    contract.policies;

  (* QoS constraints for each field of QoS vector *)
  (*
  - \forall service: \forall qos_field: \exists constraint
  *)
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
    contract.services
