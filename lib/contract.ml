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

let rec validate_regex (regex: AST.regex) (service_names_set: StringSet.t) =
  match regex with
  | AST.RService s -> StringSet.mem s service_names_set
  | AST.RConcat(r1, r2) | AST.RChoice (r1, r2) ->
      validate_regex r1 service_names_set && validate_regex r2 service_names_set
  | AST.RStar r -> validate_regex r service_names_set



let validate_contract (contract: AST.contract) =
  let service_names = List.map (fun (s: AST.service) -> s.name) contract.services in
  let service_names_set = StringSet.of_list service_names in
  
  (* no duplicate service names *)
  if List.length service_names <> StringSet.cardinal service_names_set then
    failwith "Duplicate service names found in the contract";

  List.iter (fun (policy_type, _) ->
    match policy_type with
    (* bad prefix regex use services that are defined in the program*)
    | AST.Regex regex -> 
        if not (validate_regex regex service_names_set) then
          failwith "Regex in policy uses undefined service names"
    (* policies only use services / QOS fields defined in the program *)      
    | AST.QosFieldOp (_, _, field, _) ->
        let qos_fields = "trust" :: (List.map fst contract.qos) in
        if not (List.mem field qos_fields) then
          failwith ("QoS policy uses undefined QoS field: " ^ field) 
    | AST.Sort _ -> ()
  ) contract.policies