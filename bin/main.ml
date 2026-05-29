let mode = Soteria.Symex.Approx.OX

let () =
  if Array.length Sys.argv != 3 then
    Fmt.pr "Usage: %s <contract> <orchestrator_code>\n" Sys.argv.(0)
  else
    let contract_file = Sys.argv.(1) in
    let orchestrator_file = Sys.argv.(2) in
    let contract_ast = Contract.parse contract_file in
    Contract.validate_contract contract_ast;
    let orchestrator_ast = Orchestrator.parse orchestrator_file in
    let results =
      Orchestrator.symb_run (contract_ast, orchestrator_ast) ~mode
    in
    Fmt.pr "%a" Symbolic.Runtime_pp.pp_results results
