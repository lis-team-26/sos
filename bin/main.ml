open Utils.Data

let () =
  if Array.length Sys.argv != 3 then
    Fmt.pr "Usage: %s <contract_spec> <orchestrator_code>@," Sys.argv.(0)
  else
    let contract_file = Sys.argv.(1) in
    let orchestrator_file = Sys.argv.(2) in
    let contract_ast = Contract.parse contract_file in
    let orchestrator_ast = Orchestrator.parse orchestrator_file in
    Contract.validate_contract contract_ast;
    let typed_contract_ast = Contract.type_check contract_ast |> get_or_fail in
    let typed_orchestrator_ast =
      Orchestrator.type_check contract_ast orchestrator_ast |> get_or_fail
    in
    Fmt.pr "@,Symbolic running...@,%a" Fmt.flush ();
    let results =
      Orchestrator.symb_run
        (typed_contract_ast, typed_orchestrator_ast)
        ~mode:Soteria.Symex.Approx.OX
    in
    let manifestErrors = ManifErrHeuristics.find_manif [] results
    in
    Fmt.pr "%a@.Manifest errors:@.%a" Symbolic.Runtime_pp.pp_results results Symbolic.Runtime_pp.pp_manifest_errors manifestErrors
