let mode = Soteria.Symex.Approx.OX

let () =
  if Array.length Sys.argv != 3 then
    Fmt.pr "Usage: %s <services_contract> <orchestrator_code>\n" Sys.argv.(0)
  else
    let contract_file = Sys.argv.(1) in
    let orchestrator_file = Sys.argv.(2) in
    let contract_ast = Contract.parse contract_file in
    Contract.validate_contract contract_ast;
    let orchestrator_ast = Orchestrator.parse orchestrator_file in
    let final_states =
      Orchestrator.symb_run (contract_ast, orchestrator_ast) ~mode
    in
    final_states
    |> List.mapi Orchestrator.Utils.string_of_state
    |> String.concat "\n" |> print_endline
